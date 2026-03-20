{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module IHP.OpenApiSupport
    ( ToSchema (..)
    , NamedSchema (..)
    , Schema
    , toSchema
    , genericDeclareNamedSchema
    , defaultSchemaOptions
    , OpenApiInfo (..)
    , defaultOpenApiInfo
    , buildOpenApi
    , buildOpenApiWithInfo
    ) where

import IHP.Prelude
import IHP.RouterSupport
import IHP.ViewSupport (JsonResponse)
import IHP.ModelSupport
import Data.OpenApi (ToSchema (..), NamedSchema (..), Schema, declareNamedSchema, toSchema, genericDeclareNamedSchema, defaultSchemaOptions)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Map.Strict as Map
import qualified Data.Typeable as Typeable
import Network.Wai (defaultRequest)
import qualified Control.Monad.State.Strict as State
import Data.UUID (nil)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Data
import Network.HTTP.Types.Method (StdMethod (..))

data OpenApiInfo = OpenApiInfo
    { openApiTitle :: Text
    , openApiVersion :: Text
    , openApiDescription :: Maybe Text
    }
    deriving (Eq, Show)

defaultOpenApiInfo :: forall application. Typeable.Typeable application => OpenApiInfo
defaultOpenApiInfo = OpenApiInfo
    { openApiTitle = cs (show (Typeable.typeRep (Proxy @application))) <> " API"
    , openApiVersion = "1.0.0"
    , openApiDescription = Nothing
    }

buildOpenApi :: forall application. (FrontController application, Typeable.Typeable application) => application -> JSON.Value
buildOpenApi = buildOpenApiWithInfo (defaultOpenApiInfo @application)

buildOpenApiWithInfo :: forall application. FrontController application => OpenApiInfo -> application -> JSON.Value
buildOpenApiWithInfo info application =
    let ?request = defaultRequest
        ?respond = dummyRespond
        ?application = application
    in
        let routes = router []
        in JSON.object
            [ "openapi" JSON..= ("3.0.3" :: Text)
            , "info" JSON..= openApiInfoValue info
            , "paths" JSON..= openApiPathsValue (collectPaths "" routes)
            ]
    where
        dummyRespond _ = error "buildOpenApi: response callback should never be called"

openApiInfoValue :: OpenApiInfo -> JSON.Value
openApiInfoValue OpenApiInfo { openApiTitle, openApiVersion, openApiDescription } = JSON.object
    ( [ Just ("title" JSON..= openApiTitle)
      , Just ("version" JSON..= openApiVersion)
      , ("description" JSON..=) <$> openApiDescription
      ]
        |> catMaybes
    )

type PathOperations = Map.Map Text (Map.Map Text JSON.Value)

collectPaths :: Text -> RouteDefinition -> PathOperations
collectPaths currentPrefix = \case
    RouteLeaf { routeDocumentation = UndocumentedRoute } -> mempty
    RouteLeaf { routeDocumentation = DocumentedRoute documentedRoute } -> collectDocumentedRoute currentPrefix documentedRoute
    RouteCollection routes -> routes |> map (collectPaths currentPrefix) |> mconcat
    RoutePrefix routePrefix routes ->
        let prefixedPath = appendPathPrefix currentPrefix (Text.decodeUtf8 routePrefix)
        in routes |> map (collectPaths prefixedPath) |> mconcat

collectDocumentedRoute :: Text -> DocumentedRouteInfo -> PathOperations
collectDocumentedRoute currentPrefix (AutoRouteControllerInfo { documentedActions = Nothing }) = mempty
collectDocumentedRoute currentPrefix (AutoRouteControllerInfo { documentedActions = Just docs }) =
    foldl' (insertActionOperation currentPrefix) mempty docs

insertActionOperation
    :: forall controller.
        ( AutoRoute controller
        , Data controller
        , Typeable.Typeable controller
        )
    => Text
    -> PathOperations
    -> ActionDoc controller
    -> PathOperations
insertActionOperation currentPrefix paths doc@ActionDoc { actionDocName } =
    let constructor = findControllerConstructor @controller actionDocName
        actionPath = appendPathPrefix currentPrefix (actionPrefixText @controller <> stripActionSuffixText actionDocName)
        parameters = deriveActionParameters @controller constructor
        operation = actionDocOperationValue doc parameters
        methods = allowedMethodsForAction @controller (Text.encodeUtf8 actionDocName)
    in foldl' (insertMethod actionPath operation) paths methods

findControllerConstructor :: forall controller. Data controller => Text -> Constr
findControllerConstructor actionName =
    dataTypeConstrs (dataTypeOf (undefined :: controller))
        |> find (\constructor -> cs (showConstr constructor) == actionName)
        |> fromMaybe (error ("OpenAPI docs reference unknown action " <> cs actionName))

insertMethod :: Text -> JSON.Value -> PathOperations -> StdMethod -> PathOperations
insertMethod actionPath operation paths method = Map.alter updatePath actionPath paths
    where
        methodName = httpMethodName method
        updatePath Nothing = Just (Map.singleton methodName operation)
        updatePath (Just operations) = Just (Map.insert methodName operation operations)

httpMethodName :: StdMethod -> Text
httpMethodName = \case
    GET -> "get"
    POST -> "post"
    PUT -> "put"
    DELETE -> "delete"
    OPTIONS -> "options"
    HEAD -> "head"
    PATCH -> "patch"
    TRACE -> "trace"
    CONNECT -> error "OpenAPI does not support CONNECT routes"

appendPathPrefix :: Text -> Text -> Text
appendPathPrefix currentPrefix nextPrefix =
    let normalize text
            | Text.null text = ""
            | "/" `Text.isPrefixOf` text = text
            | otherwise = "/" <> text
        trimTrailingSlash text
            | Text.length text > 1 && "/" `Text.isSuffixOf` text = Text.dropEnd 1 text
            | otherwise = text
        normalizedCurrent = currentPrefix |> normalize |> trimTrailingSlash
        normalizedNext = normalize nextPrefix
    in case (normalizedCurrent, normalizedNext) of
        ("", next) -> next
        (current, "/") -> current <> "/"
        (current, next) -> current <> next

openApiPathsValue :: PathOperations -> JSON.Value
openApiPathsValue paths =
    paths
        |> Map.toList
        |> map (\(path, operations) ->
                ( JSON.Key.fromText path
                , operations
                    |> Map.toList
                    |> map (\(method, operation) -> (JSON.Key.fromText method, operation))
                    |> JSON.KeyMap.fromList
                    |> JSON.Object
                )
            )
        |> JSON.KeyMap.fromList
        |> JSON.Object

actionDocOperationValue :: forall controller. ActionDoc controller -> [QueryParameterDocumentation] -> JSON.Value
actionDocOperationValue ActionDoc { actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView } parameters = JSON.object
    ( [ Just ("parameters" JSON..= map queryParameterValue parameters)
      , Just ("responses" JSON..= JSON.object ["200" JSON..= successResponseValue (responseSchemaValue actionDocView)])
      , ("summary" JSON..=) <$> actionDocSummary
      , ("description" JSON..=) <$> actionDocDescription
      , if null actionDocTags then Nothing else Just ("tags" JSON..= actionDocTags)
      , ("operationId" JSON..=) <$> actionDocOperationId
      , Just ("x-ihp-action" JSON..= actionDocName)
      ]
        |> catMaybes
    )

responseSchemaValue :: forall view. ToSchema (JsonResponse view) => Proxy view -> JSON.Value
responseSchemaValue _ = JSON.toJSON (toSchema (Proxy @(JsonResponse view)))

successResponseValue :: JSON.Value -> JSON.Value
successResponseValue schema = JSON.object
    [ "description" JSON..= ("Successful response" :: Text)
    , "content" JSON..= JSON.object
        [ "application/json" JSON..= JSON.object
            [ "schema" JSON..= schema
            ]
        ]
    ]

data QueryParameterDocumentation = QueryParameterDocumentation
    { parameterName :: Text
    , parameterRequired :: Bool
    , parameterSchema :: JSON.Value
    , parameterExplode :: Maybe Bool
    }

queryParameterValue :: QueryParameterDocumentation -> JSON.Value
queryParameterValue QueryParameterDocumentation { parameterName, parameterRequired, parameterSchema, parameterExplode } = JSON.object
    ( [ Just ("name" JSON..= parameterName)
      , Just ("in" JSON..= ("query" :: Text))
      , Just ("required" JSON..= parameterRequired)
      , Just ("schema" JSON..= parameterSchema)
      , if isJust parameterExplode then Just ("style" JSON..= ("form" :: Text)) else Nothing
      , ("explode" JSON..=) <$> parameterExplode
      ]
        |> catMaybes
    )

queryParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
queryParameterDocumentation parameterName =
    directParameterDocumentation @field parameterName
        <|> wrappedIdParameterDocumentation @field parameterName

directParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
directParameterDocumentation parameterName =
    asum
        [ eqT @field @Text |> fmap (\Refl -> requiredParameter @Text parameterName)
        , eqT @field @Int |> fmap (\Refl -> requiredParameter @Int parameterName)
        , eqT @field @Integer |> fmap (\Refl -> requiredParameter @Integer parameterName)
        , eqT @field @UUID |> fmap (\Refl -> requiredParameter @UUID parameterName)
        , eqT @field @(Maybe Text) |> fmap (\Refl -> optionalParameter @Text parameterName)
        , eqT @field @(Maybe Int) |> fmap (\Refl -> optionalParameter @Int parameterName)
        , eqT @field @(Maybe Integer) |> fmap (\Refl -> optionalParameter @Integer parameterName)
        , eqT @field @[Text] |> fmap (\Refl -> listParameter @Text parameterName)
        , eqT @field @[Int] |> fmap (\Refl -> listParameter @Int parameterName)
        , eqT @field @[Integer] |> fmap (\Refl -> listParameter @Integer parameterName)
        , eqT @field @[UUID] |> fmap (\Refl -> listParameter @UUID parameterName)
        ]

requiredParameter :: forall a. ToSchema a => Text -> QueryParameterDocumentation
requiredParameter parameterName = QueryParameterDocumentation
    { parameterName
    , parameterRequired = True
    , parameterSchema = JSON.toJSON (toSchema (Proxy @a))
    , parameterExplode = Nothing
    }

optionalParameter :: forall a. ToSchema a => Text -> QueryParameterDocumentation
optionalParameter parameterName = QueryParameterDocumentation
    { parameterName
    , parameterRequired = False
    , parameterSchema = JSON.toJSON (toSchema (Proxy @a))
    , parameterExplode = Nothing
    }

listParameter :: forall a. ToSchema [a] => Text -> QueryParameterDocumentation
listParameter parameterName = QueryParameterDocumentation
    { parameterName
    , parameterRequired = False
    , parameterSchema = JSON.toJSON (toSchema (Proxy @[a]))
    , parameterExplode = Just False
    }

wrappedIdParameterDocumentation :: forall field. Data field => Text -> Maybe QueryParameterDocumentation
wrappedIdParameterDocumentation parameterName
    | dataTypeName (dataTypeOf (undefined :: field)) /= "IHP.ModelSupport.Types.Id'" = Nothing
    | otherwise =
        dataTypeConstrs (dataTypeOf (undefined :: field))
            |> listToMaybe
            >>= \constructor -> either (const Nothing) Just (deriveWrappedIdParameter @field constructor parameterName)

deriveWrappedIdParameter :: forall field. Data field => Constr -> Text -> Either Text QueryParameterDocumentation
deriveWrappedIdParameter constructor parameterName =
    let nextField :: forall inner. Data inner => State.StateT (Maybe QueryParameterDocumentation) (Either Text) inner
        nextField = do
            parameter <- case queryParameterDocumentation @inner parameterName of
                Just queryParameter -> pure queryParameter
                Nothing -> State.lift (Left unsupportedInnerTypeMessage)
            State.put (Just parameter)
            case dummyValueForFieldType @inner of
                Right dummyValue -> pure dummyValue
                Left errorMessage -> State.lift (Left errorMessage)
        unsupportedInnerTypeMessage =
            "OpenAPI does not support the inner primary key type of "
            <> parameterName
    in case State.runStateT (fromConstrM nextField constructor :: State.StateT (Maybe QueryParameterDocumentation) (Either Text) field) Nothing of
        Right (_, Just parameter) -> Right parameter
        Right (_, Nothing) -> Left ("OpenAPI could not derive the inner primary key type of " <> parameterName)
        Left errorMessage -> Left errorMessage

dummyValueForFieldType :: forall field. Data field => Either Text field
dummyValueForFieldType =
    fromMaybe
        (Left unsupportedDummyTypeMessage)
        (directDummyValue @field <|> wrappedIdDummyValue @field)
    where
        unsupportedDummyTypeMessage =
            "OpenAPI dummy value is not implemented for type "
            <> cs (dataTypeName (dataTypeOf (undefined :: field)))

directDummyValue :: forall field. Data field => Maybe (Either Text field)
directDummyValue = asum
    [ eqT @field @Text |> fmap (\Refl -> Right "")
    , eqT @field @Int |> fmap (\Refl -> Right 0)
    , eqT @field @Integer |> fmap (\Refl -> Right 0)
    , eqT @field @UUID |> fmap (\Refl -> Right nil)
    , eqT @field @(Maybe Text) |> fmap (\Refl -> Right Nothing)
    , eqT @field @(Maybe Int) |> fmap (\Refl -> Right Nothing)
    , eqT @field @(Maybe Integer) |> fmap (\Refl -> Right Nothing)
    , eqT @field @[Text] |> fmap (\Refl -> Right [])
    , eqT @field @[Int] |> fmap (\Refl -> Right [])
    , eqT @field @[Integer] |> fmap (\Refl -> Right [])
    , eqT @field @[UUID] |> fmap (\Refl -> Right [])
    ]

wrappedIdDummyValue :: forall field. Data field => Maybe (Either Text field)
wrappedIdDummyValue
    | dataTypeName (dataTypeOf (undefined :: field)) /= "IHP.ModelSupport.Types.Id'" = Nothing
    | otherwise =
        dataTypeConstrs (dataTypeOf (undefined :: field))
            |> listToMaybe
            |> fmap deriveWrappedIdDummyValue

deriveWrappedIdDummyValue :: forall field. Data field => Constr -> Either Text field
deriveWrappedIdDummyValue constructor =
    let nextField :: forall inner. Data inner => State.StateT () (Either Text) inner
        nextField =
            case dummyValueForFieldType @inner of
                Right dummyValue -> pure dummyValue
                Left errorMessage -> State.lift (Left errorMessage)
    in fst <$> State.runStateT (fromConstrM nextField constructor :: State.StateT () (Either Text) field) ()

deriveActionParameters :: forall controller. Data controller => Constr -> [QueryParameterDocumentation]
deriveActionParameters constr =
    let initialState = (map cs (constrFields constr), [])
        nextField :: forall field. Data field => State.StateT ([Text], [QueryParameterDocumentation]) (Either Text) field
        nextField = do
            (remainingFields, parameters) <- State.get
            case remainingFields of
                [] -> State.lift (Left ("OpenAPI field derivation failed for action " <> cs (showConstr constr)))
                (fieldName:restFields) ->
                    case queryParameterDocumentation @field fieldName of
                        Just parameter -> do
                            State.put (restFields, parameters <> [parameter])
                            case dummyValueForFieldType @field of
                                Right dummyValue -> pure dummyValue
                                Left errorMessage -> State.lift (Left errorMessage)
                        Nothing -> State.lift (Left unsupportedTypeMessage)
                    where
                        unsupportedTypeMessage =
                            "OpenAPI does not support the AutoRoute field "
                            <> fieldName
                            <> " with type "
                            <> cs (dataTypeName (dataTypeOf (undefined :: field)))
    in case State.runStateT
            (fromConstrM nextField constr :: State.StateT ([Text], [QueryParameterDocumentation]) (Either Text) controller)
            initialState of
        Left errorMessage -> error (cs errorMessage)
        Right (_, ([], parameters)) -> parameters
        Right (_, (remainingFields, _)) ->
            error (cs ("OpenAPI field derivation did not consume all fields for action " <> cs (showConstr constr) <> ": " <> cs (show remainingFields) :: Text))

instance {-# OVERLAPPABLE #-} (KnownSymbol table, ToSchema (PrimaryKey table)) => ToSchema (Id' table) where
    declareNamedSchema _ = declareNamedSchema (Proxy @(PrimaryKey table))
