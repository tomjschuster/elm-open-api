module Language.JavaScript.Generate exposing (generate)

import Doc exposing ((|+), Doc)
import Language.JavaScript.Ast as Ast exposing (..)


--   generateExpression


generate : Ast.Program -> Doc
generate program =
    case program of
        Module moduleItems ->
            moduleItems
                |> List.map generateModuleItem
                |> Doc.concat

        Script statementList ->
            generateStatementList statementList


generateModuleItem : ModuleItem -> Doc
generateModuleItem moduleItem =
    case moduleItem of
        ImportItem declaration ->
            generateImport declaration

        ExportItem declaration ->
            generateExport declaration

        ModuleStatementListItem statementListItem ->
            generateStatementListItem statementListItem


generateImport : Import -> Doc
generateImport declaration =
    case declaration of
        Import clause specifier ->
            import_
                |+ Doc.space
                |+ generateImportClause clause
                |+ Doc.space
                |+ from
                |+ Doc.space
                |+ generateModulePath specifier

        ImportModulePath specifier ->
            import_
                |+ Doc.space
                |+ generateModulePath specifier


generateModulePath : ModulePath -> Doc
generateModulePath =
    Doc.string


generateExport : Export -> Doc
generateExport declaration =
    case declaration of
        ExportAllFrom modulePath ->
            export
                |+ Doc.space
                |+ asterisk
                |+ Doc.space
                |+ from
                |+ Doc.space
                |+ generateModulePath modulePath

        ExportFrom exportSpecifiers modulePath ->
            export
                |+ Doc.space
                |+ Doc.braces (exportSpecifiers |> List.map generateExportSpecifier |> Doc.join comma)
                |+ from
                |+ Doc.space
                |+ generateModulePath modulePath

        VariableStatementExport bindings ->
            var
                |+ Doc.space
                |+ (List.map generateBinding bindings |> Doc.join comma)

        DeclarationExport declaration ->
            export
                |+ Doc.space
                |+ generateDeclaration declaration

        DefaultHoistableDeclarationExport declaration ->
            export
                |+ Doc.space
                |+ default
                |+ Doc.space
                |+ generateHoistableDeclaration declaration

        DefaultExpressionExport expression ->
            export
                |+ Doc.space
                |+ default
                |+ Doc.space
                |+ generateExpression expression
                |+ semicolon

        DefaultClassDeclarationExport identifier heritage elements ->
            generateClassDeclaration identifier heritage elements


generateExportSpecifier : ExportSpecifier -> Doc
generateExportSpecifier specifier =
    case specifier of
        ExportSpecifier identifier ->
            generateIdentifier identifier

        AsExportSpecifier identifier asIdentifier ->
            generateIdentifier identifier
                |+ Doc.space
                |+ as_
                |+ Doc.space
                |+ generateIdentifier asIdentifier


generateImportClause : ImportClause -> Doc
generateImportClause clause =
    case clause of
        NameSpaceImport Nothing asIdentifier ->
            asterisk
                |+ Doc.space
                |+ as_
                |+ Doc.space
                |+ generateIdentifier asIdentifier

        NameSpaceImport (Just defaultIdentifier) asIdentifier ->
            generateIdentifier defaultIdentifier
                |+ comma
                |+ asterisk
                |+ Doc.space
                |+ as_
                |+ Doc.space
                |+ generateIdentifier asIdentifier

        NamedImport Nothing [] ->
            Doc.empty

        NamedImport Nothing specifiers ->
            specifiers |> List.map generateImportSpecifier |> Doc.join comma

        NamedImport (Just defaultIdentifier) [] ->
            generateIdentifier defaultIdentifier

        NamedImport (Just defaultIdentifier) specifiers ->
            generateIdentifier defaultIdentifier
                |+ comma
                |+ (specifiers |> List.map generateImportSpecifier |> Doc.join comma)


generateImportSpecifier : ImportSpecifier -> Doc
generateImportSpecifier specifier =
    case specifier of
        ImportSpecifier identifier ->
            generateIdentifier identifier

        AsImportSpecifier identifier asIdentifier ->
            generateIdentifier identifier
                |+ Doc.space
                |+ as_
                |+ Doc.space
                |+ generateIdentifier asIdentifier



-- Statements


generateStatement : Statement -> Doc
generateStatement statement =
    case statement of
        BlockStatement block ->
            generateBlock block

        Empty ->
            semicolon

        VariableStatement bindings ->
            var
                |+ Doc.space
                |+ (List.map generateBinding bindings |> Doc.join comma)

        ExpressionStatement expression ->
            generateExpression expression
                |+ semicolon

        IterationStatement iterationStatement ->
            generateIterationStatement iterationStatement

        IfStatement test consequent alternate ->
            generateIfStatement test consequent alternate

        Return returnExpression ->
            return
                |+ Doc.space
                |+ maybeGenerate generateExpression returnExpression
                |+ semicolon

        Break breakLabel ->
            break
                |+ Doc.space
                |+ maybeGenerate generateIdentifier breakLabel
                |+ semicolon

        Continue continueLabel ->
            continue
                |+ Doc.space
                |+ maybeGenerate generateIdentifier continueLabel
                |+ semicolon

        Throw expression ->
            throw
                |+ Doc.space
                |+ generateExpression expression
                |+ semicolon

        Switch expression caseClauses defaultClause ->
            generateSwitch expression caseClauses defaultClause

        TryStatement tryStatement ->
            generateTryStatement tryStatement

        Label identifier labelStatement ->
            generateIdentifier identifier
                |+ colon
                |+ generateStatement labelStatement

        Debugger ->
            debugger
                |+ semicolon


generateBlock : Block -> Doc
generateBlock (Block statementList) =
    generateStatementList statementList
        |> Doc.braces


generateStatementList : List StatementListItem -> Doc
generateStatementList statementList =
    statementList
        |> List.map generateStatementListItem
        |> Doc.concat


generateStatementListItem : StatementListItem -> Doc
generateStatementListItem statementListItem =
    case statementListItem of
        StatementItem statement ->
            generateStatement statement

        DeclarationItem declaration ->
            generateDeclaration declaration


generateIfStatement : Expression -> Statement -> Maybe Statement -> Doc
generateIfStatement test consequent alternate =
    case alternate of
        Nothing ->
            if_
                |+ Doc.parens (generateExpression test)
                |+ generateStatement consequent

        Just (IfStatement elseTest elseConsequent elseAlternate) ->
            if_
                |+ Doc.parens (generateExpression test)
                |+ generateStatement consequent
                |+ else_
                |+ Doc.space
                |+ generateIfStatement elseTest elseConsequent elseAlternate

        Just nonIfAlternate ->
            if_
                |+ Doc.parens (generateExpression test)
                |+ generateStatement consequent
                |+ else_
                |+ generateStatement nonIfAlternate


generateTryStatement : TryStatement -> Doc
generateTryStatement tryStatement =
    case tryStatement of
        TryCatch block handlerParameter handler ->
            try
                |+ generateBlock block
                |+ catch
                |+ Doc.parens (generateHandlerParameter handlerParameter)
                |+ generateBlock handler

        TryFinally block finalizer ->
            try
                |+ generateBlock block
                |+ finally
                |+ generateBlock finalizer

        TryCatchFinally block handlerParameter handler finalizer ->
            try
                |+ generateBlock block
                |+ catch
                |+ Doc.parens (generateHandlerParameter handlerParameter)
                |+ generateBlock handler
                |+ finally
                |+ generateBlock finalizer


generateHandlerParameter : TryParameter -> Doc
generateHandlerParameter tryParameter =
    case tryParameter of
        TryIdentifier identifier ->
            generateIdentifier identifier

        TryPattern pattern ->
            generatePattern pattern


generatePattern : Pattern -> Doc
generatePattern pattern =
    case pattern of
        ObjectPattern properties bindingRest ->
            (List.map generateBindingProperty properties |> Doc.join comma)
                |+ maybeGenerate generateBindingRest bindingRest
                |> Doc.braces

        ArrayPattern elements bindingRest ->
            (List.map generateBindingElement elements |> Doc.join comma)
                |+ maybeGenerate generateBindingRest bindingRest
                |> Doc.braces


generateBindingRestElement : BindingRestElement -> Doc
generateBindingRestElement element =
    case element of
        BindingRestElementPattern pattern ->
            generatePattern pattern

        BindingRestElementIdentifier identifier ->
            generateIdentifier identifier


generateBindingProperty : BindingProperty -> Doc
generateBindingProperty property =
    case property of
        SingleNameBindingProperty identifier initializer ->
            generateIdentifier identifier
                |+ maybeGenerate generateInitializer initializer

        ExpandedBindingProperty propertyName element ->
            generatePropertyName propertyName
                |+ colon
                |+ generateBindingElement element


generatePropertyName : PropertyName -> Doc
generatePropertyName propertyName =
    case propertyName of
        IdentifierProperty identifier ->
            generateIdentifier identifier

        StringProperty stringLiteral ->
            generateStringLiteral stringLiteral
                |> Doc.brackets

        NumericProperty float ->
            Doc.float float

        ComputedPropertyName expression ->
            generateExpression expression
                |> Doc.brackets


generateStringLiteral : StringLiteral -> Doc
generateStringLiteral (StringLiteral stringLiteral) =
    Doc.string stringLiteral
        |> Doc.squotes


generateBindingElement : BindingElement -> Doc
generateBindingElement element =
    case element of
        SingleNameBindingElement identifier initializer ->
            generateIdentifier identifier
                |+ maybeGenerate generateInitializer initializer

        PatternElement pattern initializer ->
            generatePattern pattern
                |+ maybeGenerate generateInitializer initializer


generateInitializer : Expression -> Doc
generateInitializer expression =
    equals
        |+ generateExpression expression


generateBindingRest : Identifier -> Doc
generateBindingRest identifier =
    elipsis
        |+ generateIdentifier identifier


generateDeclaration : Declaration -> Doc
generateDeclaration declaration =
    case declaration of
        LexicalDeclaration lexicalDeclaration ->
            generateLexicalDeclaration lexicalDeclaration

        HoistableDeclaration hoistableDeclaration ->
            generateHoistableDeclaration hoistableDeclaration

        ClassDeclaration identifier heritage elements ->
            generateClassDeclaration identifier heritage elements


generateClassDeclaration : Identifier -> Heritage -> List ClassElement -> Doc
generateClassDeclaration identifier heritage elements =
    class
        |+ Doc.space
        |+ generateIdentifier identifier
        |+ generateHeritage heritage
        |+ generateClassBody elements


generateLexicalDeclaration : LexicalDeclaration -> Doc
generateLexicalDeclaration lexicalDeclaration =
    case lexicalDeclaration of
        Const binding ->
            const
                |+ Doc.space
                |+ (List.map generateBinding binding |> Doc.join comma)

        Let binding ->
            let_
                |+ Doc.space
                |+ (List.map generateBinding binding |> Doc.join comma)


generateHoistableDeclaration : HoistableDeclaration -> Doc
generateHoistableDeclaration hoistableDeclaration =
    case hoistableDeclaration of
        FunctionDeclaration identifier parameters block ->
            function
                |+ generateParameters parameters
                |+ generateBlock block

        GeneratorDeclaration identifier parameters block ->
            function
                |+ asterisk
                |+ generateParameters parameters
                |+ generateBlock block

        AsyncFunctionDeclaration identifier parameters block ->
            async
                |+ Doc.space
                |+ function
                |+ generateParameters parameters
                |+ generateBlock block

        AsyncGeneratorDeclaration identifier parameters block ->
            async
                |+ Doc.space
                |+ function
                |+ asterisk
                |+ generateParameters parameters
                |+ generateBlock block


generateBinding : Binding -> Doc
generateBinding binding =
    case binding of
        IdentifierBinding identifier identifierExpression ->
            generateIdentifier identifier
                |+ maybeGenerate generateInitializer identifierExpression

        PatternBinding pattern expression ->
            generatePattern pattern
                |+ generateInitializer expression


generateParameters : Parameters -> Doc
generateParameters (Parameters elements restElement) =
    (List.map generateBindingElement elements |> Doc.join comma)
        |+ maybeGenerate generateBindingRestElement restElement
        |> Doc.parens


generateHeritage : Heritage -> Doc
generateHeritage heritage =
    case heritage of
        NoHeritage ->
            Doc.empty

        NullHeritage ->
            Doc.space
                |+ extends
                |+ Doc.space
                |+ null

        Heritage identifier ->
            Doc.space
                |+ extends
                |+ Doc.space
                |+ generateIdentifier identifier


generateClassBody : List ClassElement -> Doc
generateClassBody elements =
    elements
        |> List.map generateClassElement
        |> Doc.concat
        |> Doc.braces


generateClassElement : ClassElement -> Doc
generateClassElement element =
    case element of
        ClassMethod method ->
            generateMethodDefinition method

        StaticClassMethod method ->
            static
                |+ Doc.space
                |+ generateMethodDefinition method


generateMethodDefinition : MethodDefinition -> Doc
generateMethodDefinition definition =
    case definition of
        NormalMethod propertyName parameters block ->
            generatePropertyName propertyName
                |+ generateParameters parameters
                |+ generateBlock block

        GeneratorMethod propertyName parameters block ->
            generatePropertyName propertyName
                |+ asterisk
                |+ generateParameters parameters
                |+ generateBlock block

        AsyncMethod propertyName parameters block ->
            generatePropertyName propertyName
                |+ asterisk
                |+ generateParameters parameters
                |+ generateBlock block

        AsyncGeneratorMethod propertyName parameters block ->
            async
                |+ Doc.space
                |+ generatePropertyName propertyName
                |+ asterisk
                |+ generateParameters parameters
                |+ generateBlock block

        Get propertyName block ->
            get
                |+ Doc.space
                |+ generatePropertyName propertyName
                |+ Doc.parens Doc.empty
                |+ generateBlock block

        Set propertyName parameters block ->
            set
                |+ Doc.space
                |+ generatePropertyName propertyName
                |+ generateParameters parameters
                |+ generateBlock block


generateIterationStatement : IterationStatement -> Doc
generateIterationStatement iterationStatement =
    case iterationStatement of
        DoWhile statement expression ->
            do
                |+ Doc.space
                |+ generateDoWhileBody statement
                |+ while
                |+ Doc.parens (generateExpression expression)

        While expression statement ->
            while
                |+ Doc.parens (generateExpression expression)
                |+ generateStatement statement

        For declarator init test update statement ->
            for
                |+ Doc.parens (generateFor declarator init test update)
                |+ generateStatement statement

        ForIn declarator binding expression statement ->
            for
                |+ Doc.parens (generateForIn declarator binding expression)
                |+ generateStatement statement

        ForOf declarator binding expression statement ->
            for
                |+ Doc.parens (generateForOf declarator binding expression)
                |+ generateStatement statement


generateDoWhileBody : Statement -> Doc
generateDoWhileBody statement =
    case statement of
        BlockStatement _ ->
            generateStatement statement

        otherStatement ->
            generateStatement statement
                |+ Doc.line


generateFor : Declarator -> List Binding -> Maybe Expression -> Maybe Expression -> Doc
generateFor declarator init test update =
    generateForDeclarations declarator init
        |+ semicolon
        |+ maybeGenerate generateExpression test
        |+ semicolon
        |+ maybeGenerate generateExpression update


generateForDeclarations : Declarator -> List Binding -> Doc
generateForDeclarations declarator bindings =
    generateDeclarator declarator
        |+ Doc.space
        |+ (List.map generateBinding bindings |> Doc.join comma)


generateForIn : Declarator -> Binding -> Expression -> Doc
generateForIn declarator binding expression =
    generateDeclarator declarator
        |+ Doc.space
        |+ generateBinding binding
        |+ Doc.space
        |+ in_
        |+ Doc.space
        |+ generateExpression expression
        |> Doc.parens


generateForOf : Declarator -> Binding -> Expression -> Doc
generateForOf declarator binding expression =
    generateDeclarator declarator
        |+ Doc.space
        |+ generateBinding binding
        |+ Doc.space
        |+ of_
        |+ Doc.space
        |+ generateExpression expression
        |> Doc.parens


generateSwitch : Expression -> List CaseClause -> Maybe DefaultClause -> Doc
generateSwitch test caseClauses defaultClause =
    switch
        |+ Doc.parens (generateExpression test)
        |+ generateCaseClauses caseClauses defaultClause


generateCaseClauses : List CaseClause -> Maybe DefaultClause -> Doc
generateCaseClauses clauses defaultClause =
    (List.map generateCaseClause clauses |> Doc.join Doc.empty)
        |+ maybeGenerate generateDefaultClause defaultClause
        |> Doc.braces


generateCaseClause : CaseClause -> Doc
generateCaseClause ( expression, statementList ) =
    case_
        |+ Doc.space
        |+ generateExpression expression
        |+ colon
        |+ generateStatementList statementList


generateDefaultClause : DefaultClause -> Doc
generateDefaultClause statementList =
    default
        |+ colon
        |+ generateStatementList statementList


generateExpression : Expression -> Doc
generateExpression expression =
    case expression of
        This ->
            this

        IdentifierExpression identifier ->
            generateIdentifier identifier

        LiteralExpression literal ->
            generateLiteral literal

        ObjectLiteral propertyDefinitions ->
            propertyDefinitions
                |> List.map generatePropertyDefinition
                |> Doc.join comma
                |> Doc.braces

        ArrayLiteral elements ->
            elements
                |> List.map generateArrayElement
                |> Doc.join comma
                |> Doc.brackets

        TemplateLiteral templateItems ->
            templateItems
                |> List.map generateTemplateItem
                |> Doc.concat
                |> backticks

        UnaryExpression operator operand ->
            case operator of
                Delete ->
                    delete
                        |+ Doc.space
                        |+ generateExpression operand

                Void ->
                    void
                        |+ Doc.parens (generateExpression operand)

                TypeOf ->
                    typeOf
                        |+ Doc.space
                        |+ generateExpression operand

                UnaryPlus ->
                    plus
                        |+ generateExpression operand

                UnaryNegation ->
                    minus
                        |+ generateExpression operand

                BitwiseNot ->
                    tilde
                        |+ generateExpression operand

                LogicalNot ->
                    exclamationPoint
                        |+ generateExpression operand

        UpdateExpression operatorPosition operator leftHandSideExpression ->
            case operatorPosition of
                Prefix ->
                    generateUpdateOperator operator
                        |+ generateLeftHandSideExpression leftHandSideExpression

                Postfix ->
                    generateLeftHandSideExpression leftHandSideExpression
                        |+ generateUpdateOperator operator

        BinaryExpression leftOperand operator rightOperand ->
            case operator of
                In ->
                    generateExpression leftOperand
                        |+ Doc.space
                        |+ generateBinaryOperator operator
                        |+ Doc.space
                        |+ generateExpression rightOperand

                InstanceOf ->
                    generateExpression leftOperand
                        |+ Doc.space
                        |+ generateBinaryOperator operator
                        |+ Doc.space
                        |+ generateExpression rightOperand

                otherOperator ->
                    generateExpression leftOperand
                        |+ generateBinaryOperator otherOperator
                        |+ generateExpression rightOperand

        ConditionalExpression test consequent alternate ->
            generateExpression test
                |+ questionMark
                |+ generateExpression consequent
                |+ colon
                |+ generateExpression alternate

        GroupingExpression expression ->
            Doc.parens (generateExpression expression)

        AssignmentExpression leftHandSideExpression operator expression ->
            generateLeftHandSideExpression leftHandSideExpression
                |+ generateAssignmentOperator operator
                |+ generateExpression expression

        FunctionExpression expression ->
            generateFunctionExpression expression

        MemberExpression expression ->
            generateMemberExpression expression

        CallExpression callee arguments ->
            generateCallee callee
                |+ generateArguments arguments

        NewExpression identifier arguments ->
            new
                |+ Doc.space
                |+ generateIdentifier identifier
                |+ maybeGenerate generateArguments arguments

        Await expression ->
            await
                |+ Doc.space
                |+ generateExpression expression

        Yield expression ->
            yield
                |+ Doc.space
                |+ generateExpression expression

        YieldGenerator expression ->
            yield
                |+ asterisk
                |+ Doc.space
                |+ generateExpression expression


generateLiteral : Literal -> Doc
generateLiteral literal =
    case literal of
        NullLiteral ->
            null

        BooleanLiteral bool ->
            Doc.bool bool

        Numeric float ->
            Doc.float float

        StringLiteralExpression string ->
            generateStringLiteral string


generatePropertyDefinition : PropertyDefinition -> Doc
generatePropertyDefinition propertyDefinition =
    case propertyDefinition of
        KeyValueProperty name expression ->
            generatePropertyName name
                |+ colon
                |+ generateExpression expression

        MethodProperty methodDefinition ->
            generateMethodDefinition methodDefinition

        SpreadProperty expression ->
            elipsis
                |+ generateExpression expression

        ShortHandProperty identifier ->
            generateIdentifier identifier


generateArrayElement : ArrayElement -> Doc
generateArrayElement element =
    case element of
        ExpressionElement expression ->
            generateExpression expression

        SpreadElement expression ->
            elipsis
                |+ generateExpression expression


generateTemplateItem : TemplateItem -> Doc
generateTemplateItem item =
    case item of
        TemplateString string ->
            Doc.string string

        TemplateSubstitution expression ->
            dollarSign
                |+ Doc.braces (generateExpression expression)


generateUpdateOperator : UpdateOperator -> Doc
generateUpdateOperator operator =
    case operator of
        Increment ->
            plus |+ plus

        Decrement ->
            minus |+ minus


generateBinaryOperator : BinaryOperator -> Doc
generateBinaryOperator binaryOperator =
    case binaryOperator of
        Addition ->
            plus

        Subtraction ->
            minus

        Multiplication ->
            asterisk

        Division ->
            slash

        Exponentiation ->
            asterisk |+ asterisk

        BitwiseAND ->
            ampersand

        BitwiseXOR ->
            carrot

        BitwiseOR ->
            pipe

        LeftShift ->
            leftCarrot |+ leftCarrot

        RightShift ->
            rightCarrot |+ rightCarrot

        UnsignedRightShift ->
            rightCarrot |+ rightCarrot |+ rightCarrot

        And ->
            ampersand |+ ampersand

        Or ->
            pipe |+ pipe

        Equality ->
            equals |+ equals

        Inequality ->
            exclamationPoint |+ equals

        StrictEquality ->
            equals |+ equals |+ equals

        StrictInequality ->
            exclamationPoint |+ equals |+ equals

        GreaterThan ->
            rightCarrot

        GreaterThanOrEqual ->
            rightCarrot |+ equals

        LessThan ->
            leftCarrot

        LessThanOrEqual ->
            leftCarrot |+ equals

        InstanceOf ->
            instanceOf

        In ->
            in_


generateAssignmentOperator : AssignmentOperator -> Doc
generateAssignmentOperator operator =
    case operator of
        Assignment ->
            equals

        AdditionAssignment ->
            plus |+ equals

        SubtractionAssignment ->
            minus |+ equals

        MultiplicationAssignment ->
            asterisk |+ equals

        DivisionAssignment ->
            slash |+ equals

        RemainderAssignment ->
            percent |+ equals

        ExponentiationAssignment ->
            asterisk |+ asterisk |+ equals

        LeftShiftAssignment ->
            leftCarrot |+ leftCarrot |+ equals

        RightShiftAssignment ->
            rightCarrot |+ rightCarrot |+ equals

        UnsignedRightShiftAssignment ->
            rightCarrot |+ rightCarrot |+ rightCarrot |+ equals

        BitwiseANDAssignment ->
            ampersand |+ equals

        BitwiseXORAssignment ->
            carrot |+ equals

        BitwiseORAssignment ->
            pipe |+ equals


generateLeftHandSideExpression : LeftHandSideExpression -> Doc
generateLeftHandSideExpression leftHandSideExpression =
    case leftHandSideExpression of
        LeftHandSideIdentifier identifier ->
            generateIdentifier identifier

        LeftHandSideMemberExpression memberExpression ->
            generateMemberExpression memberExpression

        LeftHandSidePattern pattern ->
            generatePattern pattern

        LeftHandSideSuperProperty property ->
            generateProperty property

        LeftHandSideNewTarget ->
            new |+ dot |+ target


generateDeclarator : Declarator -> Doc
generateDeclarator declarator =
    case declarator of
        ConstDeclarator ->
            const

        LetDeclarator ->
            let_

        VarDeclarator ->
            var


generateFunctionExpression : FunctionExpression -> Doc
generateFunctionExpression expression =
    case expression of
        NormalFunctionExpression name parameters block ->
            function
                |+ Doc.space
                |+ maybeGenerate generateIdentifier name
                |+ generateParameters parameters
                |+ generateBlock block

        ArrowFunctionExpression parameters body ->
            generateArrowParameters parameters
                |+ fatArrow
                |+ generateArrowBody body

        AsyncFunctionExpression name parameters block ->
            async
                |+ Doc.space
                |+ function
                |+ Doc.space
                |+ maybeGenerate generateIdentifier name
                |+ generateParameters parameters
                |+ generateBlock block

        GeneratorExpression name parameters block ->
            asterisk
                |+ function
                |+ Doc.space
                |+ maybeGenerate generateIdentifier name
                |+ generateParameters parameters
                |+ generateBlock block

        AsyncGeneratorExpression name parameters block ->
            async
                |+ Doc.space
                |+ asterisk
                |+ function
                |+ Doc.space
                |+ maybeGenerate generateIdentifier name
                |+ generateParameters parameters
                |+ generateBlock block

        ClassExpression name heritage elements ->
            class
                |+ Doc.space
                |+ maybeGenerate generateIdentifier name
                |+ generateHeritage heritage
                |+ generateClassBody elements


generateArrowParameters : ArrowFunctionParameters -> Doc
generateArrowParameters parameters =
    case parameters of
        UnparenthesizedArrowFunctionParam identifier ->
            generateIdentifier identifier

        NormalArrowFunctionParams normalParameters ->
            generateParameters normalParameters


generateArrowBody : ArrowFunctionBody -> Doc
generateArrowBody body =
    case body of
        ConciseArrowFunctionBody expression ->
            generateExpression expression

        NormalArrowFunctionBody block ->
            generateBlock block


generateMemberExpression : MemberExpression -> Doc
generateMemberExpression (Member expression property) =
    generateExpression expression
        |+ generateProperty property


generateProperty : Property -> Doc
generateProperty property =
    case property of
        ComputedProperty expression ->
            Doc.brackets (generateExpression expression)

        DotProperty identifier ->
            dot
                |+ generateIdentifier identifier


generateCallee : Callee -> Doc
generateCallee callee =
    case callee of
        IdentifierCallee identifier ->
            generateIdentifier identifier

        MemberExpressionCallee memberExpression ->
            generateMemberExpression memberExpression

        FunctionExpressionCallee functionExpression ->
            generateFunctionExpression functionExpression

        SuperCallee ->
            super


generateArguments : Arguments -> Doc
generateArguments arguments =
    case arguments of
        Arguments [] (Just rest) ->
            Doc.parens (elipsis |+ generateExpression rest)

        Arguments expressions restExpression ->
            Doc.parens
                ((List.map generateExpression expressions |> Doc.join comma)
                    |+ maybeGenerate
                        (\x -> comma |+ elipsis |+ generateExpression x)
                        restExpression
                )


generateIdentifier : Identifier -> Doc
generateIdentifier =
    Doc.string


maybeGenerate : (a -> Doc) -> Maybe a -> Doc
maybeGenerate f maybe =
    maybe
        |> Maybe.map f
        |> Maybe.withDefault Doc.empty


import_ : Doc
import_ =
    Doc.string "import"


export : Doc
export =
    Doc.string "export"


as_ : Doc
as_ =
    Doc.string "as"


from : Doc
from =
    Doc.string "from"


return : Doc
return =
    Doc.string "return"


break : Doc
break =
    Doc.string "break"


continue : Doc
continue =
    Doc.string "continue"


throw : Doc
throw =
    Doc.string "throw"


await : Doc
await =
    Doc.string "await"


yield : Doc
yield =
    Doc.string "yield"


debugger : Doc
debugger =
    Doc.string "debugger"


null : Doc
null =
    Doc.string "null"


let_ : Doc
let_ =
    Doc.string "let"


const : Doc
const =
    Doc.string "const"


var : Doc
var =
    Doc.string "var"


function : Doc
function =
    Doc.string "function"


async : Doc
async =
    Doc.string "async"


class : Doc
class =
    Doc.string "class"


extends : Doc
extends =
    Doc.string "extends"


static : Doc
static =
    Doc.string "static"


get : Doc
get =
    Doc.string "get"


set : Doc
set =
    Doc.string "set"


if_ : Doc
if_ =
    Doc.string "if"


else_ : Doc
else_ =
    Doc.string "else"


switch : Doc
switch =
    Doc.string "switch"


case_ : Doc
case_ =
    Doc.string "case"


default : Doc
default =
    Doc.string "defualt"


do : Doc
do =
    Doc.string "do"


while : Doc
while =
    Doc.string "while"


for : Doc
for =
    Doc.string "for"


in_ : Doc
in_ =
    Doc.string "in"


of_ : Doc
of_ =
    Doc.string "of"


try : Doc
try =
    Doc.string "try"


catch : Doc
catch =
    Doc.string "catch"


finally : Doc
finally =
    Doc.string "finally"


new : Doc
new =
    Doc.string "new"


this : Doc
this =
    Doc.string "this"


target : Doc
target =
    Doc.string "target"


super : Doc
super =
    Doc.string "super"


equals : Doc
equals =
    Doc.char '='


dot : Doc
dot =
    Doc.char '.'


comma : Doc
comma =
    Doc.char ','


colon : Doc
colon =
    Doc.char ':'


semicolon : Doc
semicolon =
    Doc.char ';'


elipsis : Doc
elipsis =
    Doc.string "..."


asterisk : Doc
asterisk =
    Doc.char '*'


questionMark : Doc
questionMark =
    Doc.char '?'


dollarSign : Doc
dollarSign =
    Doc.char '$'


slash : Doc
slash =
    Doc.char '/'


ampersand : Doc
ampersand =
    Doc.char '&'


pipe : Doc
pipe =
    Doc.char '|'


carrot : Doc
carrot =
    Doc.char '^'


leftCarrot : Doc
leftCarrot =
    Doc.char '<'


rightCarrot : Doc
rightCarrot =
    Doc.char '>'


fatArrow : Doc
fatArrow =
    Doc.string "=>"


backtick : Doc
backtick =
    Doc.char '`'


backticks : Doc -> Doc
backticks doc =
    Doc.surround backtick backtick doc


delete : Doc
delete =
    Doc.string "delete"


void : Doc
void =
    Doc.string "void"


typeOf : Doc
typeOf =
    Doc.string "typeof "


plus : Doc
plus =
    Doc.char '+'


minus : Doc
minus =
    Doc.char '-'


tilde : Doc
tilde =
    Doc.char '~'


percent : Doc
percent =
    Doc.char '%'


exclamationPoint : Doc
exclamationPoint =
    Doc.char '!'


instanceOf : Doc
instanceOf =
    Doc.string "instanceof"
