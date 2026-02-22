{-|
Module: IHP.LocalFirst.CodeGen
Description: Source discovery and artifact generation for local-first actions
Copyright: (c) digitally induced GmbH, 2026
-}
module IHP.LocalFirst.CodeGen where

import IHP.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import GHC.Generics (Generic)
import IHP.LocalFirst.Safety

data LocalRouteDefinition = LocalRouteDefinition
    { moduleName :: !Text
    , actionName :: !Text
    , sourceFile :: !FilePath
    } deriving (Eq, Show, Generic)

instance Aeson.ToJSON LocalRouteDefinition

data LocalFieldType
    = LocalFieldText
    | LocalFieldBool
    deriving (Eq, Show)

data LocalGeneratedAction
    = LocalGeneratedUpdateAction
        { actionName :: !Text
        , routePath :: !Text
        , tableName :: !Text
        , idField :: !Text
        , fields :: ![(Text, Text, LocalFieldType)] -- (record field, form field, form field type)
        }
    | LocalGeneratedCreateAction
        { actionName :: !Text
        , routePath :: !Text
        , tableName :: !Text
        , fields :: ![(Text, Text, LocalFieldType)] -- (record field, form field, form field type)
        }
    deriving (Eq, Show)

discoverLocalRoutes :: FilePath -> IO [LocalRouteDefinition]
discoverLocalRoutes projectRoot = do
    sourceFiles <- findHaskellSourceFiles projectRoot
    discovered <- mapM discoverLocalRoutesInFile sourceFiles
    pure (concat discovered)

discoverLocalRoutesInFile :: FilePath -> IO [LocalRouteDefinition]
discoverLocalRoutesInFile file = do
    source <- Text.readFile file
    let moduleName = detectModuleName source
    let linesWithNumbers = zip [1 ..] (Text.lines source)
    pure (linesWithNumbers |> mapMaybe (extractLocalAction moduleName file))

discoverGeneratedLocalActions :: FilePath -> IO [LocalGeneratedAction]
discoverGeneratedLocalActions projectRoot = do
    sourceFiles <- findHaskellSourceFiles projectRoot
    discovered <- mapM discoverGeneratedLocalActionsInFile sourceFiles
    pure (concat discovered)

discoverGeneratedLocalActionsInFile :: FilePath -> IO [LocalGeneratedAction]
discoverGeneratedLocalActionsInFile file = do
    source <- Text.readFile file
    pure (source |> Text.lines |> zip [1 ..] |> toActionBlocks |> mapMaybe parseGeneratedAction)

writeLocalRouteArtifacts :: FilePath -> IO [LocalSafetyViolation]
writeLocalRouteArtifacts projectRoot = do
    sourceFiles <- findHaskellSourceFiles projectRoot
    safetyViolations <- sourceFiles |> mapM (\file -> do
        source <- Text.readFile file
        pure (scanLocalSafetySource file source))
    localRoutes <- discoverLocalRoutes projectRoot
    generatedActions <- discoverGeneratedLocalActions projectRoot

    let generatedDirectory = projectRoot FilePath.</> "Generated"
    Directory.createDirectoryIfMissing True generatedDirectory

    let generatedModulePath = generatedDirectory FilePath.</> "LocalRoutes.hs"
    Text.writeFile generatedModulePath (renderGeneratedLocalRoutesModule localRoutes)

    let manifestPath = projectRoot FilePath.</> "local-routes.manifest.json"
    LBS.writeFile manifestPath (Aeson.encode localRoutes)

    let staticDirectory = projectRoot FilePath.</> "static"
    Directory.createDirectoryIfMissing True staticDirectory
    let generatedJsPath = staticDirectory FilePath.</> "ihp-local-routes.js"
    Text.writeFile generatedJsPath (renderGeneratedLocalRoutesScript generatedActions)

    pure (concat safetyViolations)

renderGeneratedLocalRoutesModule :: [LocalRouteDefinition] -> Text
renderGeneratedLocalRoutesModule localRoutes =
    let
        routeValues = localRoutes
            |> map (\LocalRouteDefinition { moduleName, actionName } -> "(\"" <> moduleName <> "\", \"" <> actionName <> "\")")
            |> Text.intercalate "\n    , "
        routeValuesWithBrackets =
            if null localRoutes
                then "[]"
                else "[ " <> routeValues <> "\n    ]"
    in
        Text.unlines
            [ "module Generated.LocalRoutes where"
            , ""
            , "import IHP.Prelude"
            , ""
            , "localRoutes :: [(Text, Text)]"
            , "localRoutes ="
            , "    " <> routeValuesWithBrackets
            ]

findHaskellSourceFiles :: FilePath -> IO [FilePath]
findHaskellSourceFiles root = do
    entries <- Directory.listDirectory root
    fmap concat (mapM (visitEntry root) entries)
    where
        skipDirectories = Set.fromList
            [ ".git"
            , ".direnv"
            , ".devenv"
            , "dist"
            , "dist-newstyle"
            , "build"
            , "result"
            , "node_modules"
            ]

        visitEntry currentRoot entry = do
            let fullPath = currentRoot FilePath.</> entry
            isDirectory <- Directory.doesDirectoryExist fullPath
            if isDirectory
                then
                    if Set.member entry skipDirectories
                        then pure []
                        else findHaskellSourceFiles fullPath
                else
                    if FilePath.takeExtension entry == ".hs"
                        then pure [fullPath]
                        else pure []

extractLocalAction :: Text -> FilePath -> (Int, Text) -> Maybe LocalRouteDefinition
extractLocalAction moduleName file (lineNumber, lineText)
    | not ("action " `Text.isInfixOf` lineText) = Nothing
    | not ("local do" `Text.isInfixOf` lineText || "localWith" `Text.isInfixOf` lineText) = Nothing
    | otherwise = do
        actionName <- parseActionNameFromLine lineText
        Just LocalRouteDefinition { moduleName, actionName, sourceFile = file <> ":" <> cs (tshow lineNumber) }
    where
        parseActionNameFromLine :: Text -> Maybe Text
        parseActionNameFromLine textLine =
            let
                afterKeyword = snd (Text.breakOn "action " textLine)
            in
                if Text.null afterKeyword
                    then Nothing
                    else
                        afterKeyword
                            |> Text.drop (Text.length ("action " :: Text))
                            |> Text.takeWhile (\char -> char /= ' ' && char /= '{' && char /= '=')
                            |> \name -> if Text.null name then Nothing else Just name

detectModuleName :: Text -> Text
detectModuleName source =
    source
        |> Text.lines
        |> find ("module " `Text.isPrefixOf`)
        |> maybe "Unknown.Module" (\line ->
            line
                |> Text.drop (Text.length ("module " :: Text))
                |> Text.takeWhile (/= ' ')
        )

data ActionBlock = ActionBlock
    { actionHeaderLine :: !Int
    , actionHeader :: !Text
    , actionBody :: ![Text]
    } deriving (Eq, Show)

toActionBlocks :: [(Int, Text)] -> [ActionBlock]
toActionBlocks linesWithNumbers = go linesWithNumbers Nothing []
    where
        go [] Nothing blocks = reverse blocks
        go [] (Just (lineNumber, header, body)) blocks =
            reverse (ActionBlock { actionHeaderLine = lineNumber, actionHeader = header, actionBody = reverse body } : blocks)
        go ((lineNumber, lineText):rest) current blocks
            | isActionStart lineText =
                case current of
                    Nothing -> go rest (Just (lineNumber, lineText, [])) blocks
                    Just (previousLineNumber, previousHeader, previousBody) ->
                        let
                            previousActionBlock = ActionBlock
                                { actionHeaderLine = previousLineNumber
                                , actionHeader = previousHeader
                                , actionBody = reverse previousBody
                                }
                        in
                            go rest (Just (lineNumber, lineText, [])) (previousActionBlock : blocks)
            | otherwise =
                case current of
                    Nothing -> go rest Nothing blocks
                    Just (currentLineNumber, currentHeader, currentBody) ->
                        go rest (Just (currentLineNumber, currentHeader, lineText : currentBody)) blocks

        isActionStart lineText = "action " `Text.isPrefixOf` Text.stripStart lineText

parseGeneratedAction :: ActionBlock -> Maybe LocalGeneratedAction
parseGeneratedAction ActionBlock { actionHeader, actionBody }
    | not ("local do" `Text.isInfixOf` actionHeader || "localWith" `Text.isInfixOf` actionHeader) = Nothing
    | otherwise = do
        actionName <- parseActionNameFromHeader actionHeader
        parseGeneratedUpdateAction actionName actionBody <|> parseGeneratedCreateAction actionName actionBody

parseGeneratedUpdateAction :: Text -> [Text] -> Maybe LocalGeneratedAction
parseGeneratedUpdateAction actionName actionBody = do
    (recordVar, idVar) <- actionBody |> mapMaybe parseFetchBinding |> listToMaybe
    let parameterMap = actionBody |> mapMaybe parseParamBinding |> Map.fromList
    let setBindings = actionBody |> mapMaybe parseSetBinding
    if null setBindings
        then Nothing
        else
            if not (any (\line -> "|> updateRecord" `Text.isInfixOf` line) actionBody)
                then Nothing
                else do
                    let routePath = "/" <> stripActionSuffix actionName
                    let tableName = guessTableNameFromRecordVariable recordVar
                    let idField = normalizeIdentifier idVar
                    let fields = setBindings |> map \(fieldName, variableName) ->
                            case Map.lookup variableName parameterMap of
                                Just (formFieldName, localFieldType) -> (fieldName, formFieldName, localFieldType)
                                Nothing -> (fieldName, normalizeIdentifier variableName, LocalFieldText)

                    Just LocalGeneratedUpdateAction
                        { actionName
                        , routePath
                        , tableName
                        , idField
                        , fields
                        }

parseGeneratedCreateAction :: Text -> [Text] -> Maybe LocalGeneratedAction
parseGeneratedCreateAction actionName actionBody = do
    recordVar <- actionBody |> mapMaybe parseNewRecordBinding |> listToMaybe
    let parameterMap = actionBody |> mapMaybe parseParamBinding |> Map.fromList
    let setBindings = actionBody |> mapMaybe parseSetBinding
    if null setBindings
        then Nothing
        else
            if not (any (\line -> "|> createRecord" `Text.isInfixOf` line) actionBody)
                then Nothing
                else do
                    let routePath = "/" <> stripActionSuffix actionName
                    let tableName = guessTableNameFromRecordVariable recordVar
                    let fields = setBindings |> map \(fieldName, variableName) ->
                            case Map.lookup variableName parameterMap of
                                Just (formFieldName, localFieldType) -> (fieldName, formFieldName, localFieldType)
                                Nothing -> (fieldName, normalizeIdentifier variableName, LocalFieldText)

                    Just LocalGeneratedCreateAction
                        { actionName
                        , routePath
                        , tableName
                        , fields
                        }

parseActionNameFromHeader :: Text -> Maybe Text
parseActionNameFromHeader header =
    let
        afterKeyword = snd (Text.breakOn "action " header)
    in
        if Text.null afterKeyword
            then Nothing
            else
                afterKeyword
                    |> Text.drop (Text.length ("action " :: Text))
                    |> Text.takeWhile (\char -> char /= ' ' && char /= '{' && char /= '=')
                    |> normalizeIdentifier
                    |> \name -> if Text.null name then Nothing else Just name

parseFetchBinding :: Text -> Maybe (Text, Text)
parseFetchBinding line =
    case Text.words (Text.strip line) of
        [recordVar, "<-", "fetch", idVar] -> Just (normalizeIdentifier recordVar, normalizeIdentifier idVar)
        _ -> Nothing

parseNewRecordBinding :: Text -> Maybe Text
parseNewRecordBinding line =
    let
        trimmed = Text.strip line
    in
        if not ("let " `Text.isPrefixOf` trimmed)
            then Nothing
            else do
                let afterLet = Text.drop (Text.length ("let " :: Text)) trimmed
                let (recordVarRaw, rest) = Text.breakOn "=" afterLet
                let recordVar = normalizeIdentifier recordVarRaw
                if Text.null recordVar
                    then Nothing
                    else
                        if "newRecord" `Text.isInfixOf` rest
                            then Just recordVar
                            else Nothing

parseParamBinding :: Text -> Maybe (Text, (Text, LocalFieldType))
parseParamBinding line =
    let
        trimmed = Text.strip line
    in
        if not ("let " `Text.isPrefixOf` trimmed)
            then Nothing
            else do
                let variableName = trimmed |> Text.drop 4 |> Text.takeWhile (\char -> Char.isAlphaNum char || char == '_' || char == '\'') |> normalizeIdentifier
                if Text.null variableName
                    then Nothing
                    else do
                        formField <- extractFirstQuotedText trimmed
                        let localFieldType =
                                if "paramOrDefault False" `Text.isInfixOf` trimmed || "param @Bool" `Text.isInfixOf` trimmed
                                    then LocalFieldBool
                                    else LocalFieldText
                        pure (variableName, (formField, localFieldType))

parseSetBinding :: Text -> Maybe (Text, Text)
parseSetBinding line =
    let
        trimmed = Text.strip line
    in
        if not ("|> set #" `Text.isPrefixOf` trimmed)
            then Nothing
            else
                let
                    afterHash = Text.drop (Text.length ("|> set #" :: Text)) trimmed
                    fieldName = afterHash |> Text.takeWhile (\char -> Char.isAlphaNum char || char == '_')
                    remaining = afterHash |> Text.dropWhile (\char -> Char.isAlphaNum char || char == '_') |> Text.strip
                    valueVariable = normalizeIdentifier remaining
                in
                    if Text.null fieldName || Text.null valueVariable
                        then Nothing
                        else Just (fieldName, valueVariable)

extractFirstQuotedText :: Text -> Maybe Text
extractFirstQuotedText input =
    let
        (_, afterFirstQuoteWithQuote) = Text.breakOn "\"" input
    in
        if Text.null afterFirstQuoteWithQuote
            then Nothing
            else
                let
                    afterFirstQuote = Text.drop 1 afterFirstQuoteWithQuote
                    (quotedContent, _) = Text.breakOn "\"" afterFirstQuote
                in
                    if Text.null quotedContent
                        then Nothing
                        else Just quotedContent

normalizeIdentifier :: Text -> Text
normalizeIdentifier =
    Text.takeWhile (\char -> Char.isAlphaNum char || char == '_' || char == '\'')
        . Text.strip

stripActionSuffix :: Text -> Text
stripActionSuffix actionName =
    if "Action" `Text.isSuffixOf` actionName
        then Text.dropEnd (Text.length ("Action" :: Text)) actionName
        else actionName

guessTableNameFromRecordVariable :: Text -> Text
guessTableNameFromRecordVariable recordVariable =
    let
        normalized = normalizeIdentifier recordVariable |> Text.toLower
    in
        if Text.null normalized
            then "records"
            else
                if "y" `Text.isSuffixOf` normalized
                    then Text.dropEnd 1 normalized <> "ies"
                    else
                        if "s" `Text.isSuffixOf` normalized
                            then normalized <> "es"
                            else normalized <> "s"

renderGeneratedLocalRoutesScript :: [LocalGeneratedAction] -> Text
renderGeneratedLocalRoutesScript actions =
    Text.unlines
        ([ "(function () {"
         , "    if (!window.IHPLocalRuntime || !window.IHPLocalRuntime.registerAction) {"
         , "        return;"
         , "    }"
         ]
         <> (actions |> concatMap renderAction)
         <> [ "})();"
            ]
        )
    where
        renderAction :: LocalGeneratedAction -> [Text]
        renderAction LocalGeneratedUpdateAction { routePath, tableName, idField, fields } =
            [ ""
            , "    window.IHPLocalRuntime.registerAction(" <> renderJsString routePath <> ", async ({ formFields }) => {"
            , "        await window.IHPLocalRuntime.updateRecord(" <> renderJsString tableName <> ", " <> renderFormFieldValue idField <> ", {"
            ]
            <> (fields |> map renderField)
            <> [ "        });"
               , "    }, { methods: ['POST'] });"
               , "    if (window.IHPLocalRuntime.registerDomSnapshot) {"
               , "        window.IHPLocalRuntime.registerDomSnapshot(" <> renderJsString routePath <> ", {"
               , "            table: " <> renderJsString tableName <> ","
               , "            idField: " <> renderJsString idField <> ","
               , "            fields: ["
               ]
            <> (fields |> map renderDomSnapshotField)
            <> [ "            ],"
               , "        });"
               , "    }"
               ]
        renderAction LocalGeneratedCreateAction { routePath, tableName, fields } =
            [ ""
            , "    window.IHPLocalRuntime.registerAction(" <> renderJsString routePath <> ", async ({ formFields }) => {"
            , "        await window.IHPLocalRuntime.createRecord(" <> renderJsString tableName <> ", {"
            ]
            <> (fields |> map renderField)
            <> [ "        });"
               , "    }, { methods: ['POST'] });"
               ]

        renderField :: (Text, Text, LocalFieldType) -> Text
        renderField (fieldName, formFieldName, localFieldType) =
            "            " <> renderJsString (fieldNameToColumnName fieldName) <> ": " <> renderFormValueExpression formFieldName localFieldType <> ","

        renderDomSnapshotField :: (Text, Text, LocalFieldType) -> Text
        renderDomSnapshotField (fieldName, formFieldName, localFieldType) =
            "                { column: "
                <> renderJsString (fieldNameToColumnName fieldName)
                <> ", formField: "
                <> renderJsString formFieldName
                <> ", fieldType: "
                <> renderJsString (renderLocalFieldType localFieldType)
                <> " },"

        renderLocalFieldType :: LocalFieldType -> Text
        renderLocalFieldType localFieldType = case localFieldType of
            LocalFieldText -> "text"
            LocalFieldBool -> "bool"

        renderFormFieldValue :: Text -> Text
        renderFormFieldValue formFieldName = "formFields[" <> renderJsString formFieldName <> "]"

        renderFormValueExpression :: Text -> LocalFieldType -> Text
        renderFormValueExpression formFieldName localFieldType = case localFieldType of
            LocalFieldText -> "(" <> renderFormFieldValue formFieldName <> " || '')"
            LocalFieldBool ->
                "((" <> renderFormFieldValue formFieldName <> " === true)"
                <> " || (" <> renderFormFieldValue formFieldName <> " === 'true')"
                <> " || (" <> renderFormFieldValue formFieldName <> " === 'on')"
                <> " || (" <> renderFormFieldValue formFieldName <> " === '1'))"

        renderJsString :: Text -> Text
        renderJsString value =
            "'" <> (value |> Text.replace "\\" "\\\\" |> Text.replace "'" "\\'") <> "'"
