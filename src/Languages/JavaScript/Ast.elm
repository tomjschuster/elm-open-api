module Languages.JavaScript.Ast exposing (..)

import Doc exposing ((|+), Doc)


produce : Statement -> Doc
produce statement =
    case statement of
        StatementList statements ->
            statements |> List.map produce |> Doc.concat

        BlockStatement statements ->
            statements |> List.map produce |> Doc.concat |> Doc.braces

        ExpressionStatement expression ->
            produceExpression expression

        IfStatement test consesquent alternate ->
            produceIfTest test
                |+ produceBlock consesquent
                |+ produceIfAlternate alternate

        VariableDeclaration kind declarators ->
            produceDeclarationKind kind
                |+ Doc.space
                |+ produceDeclarators declarators
                |+ Doc.char ';'

        FunctionDeclaration params body ->
            Doc.string "function"
                |+ produceArguments params
                |+ produceBlock body

        NamedFunctionDeclaration name params body ->
            Doc.string "function"
                |+ Doc.space
                |+ produceIdentifier name
                |+ produceArguments params
                |+ produceBlock body


produceIfTest : Expression -> Doc
produceIfTest test =
    Doc.string "if"
        |+ Doc.parens (produceExpression test)


produceBlock : Statement -> Doc
produceBlock statement =
    case statement of
        BlockStatement statements ->
            statements
                |> List.map produce
                |> Doc.concat
                |> Doc.braces

        otherStatement ->
            Doc.braces (produce otherStatement)


produceIfAlternate : Statement -> Doc
produceIfAlternate alternate =
    case alternate of
        IfStatement _ _ _ ->
            Doc.space
                |+ produce alternate

        otherStatement ->
            produceBlock alternate


produceDeclarationKind : DeclarationKind -> Doc
produceDeclarationKind =
    declarationKindToString >> Doc.string


declarationKindToString : DeclarationKind -> String
declarationKindToString kind =
    case kind of
        Var ->
            "var"

        Let ->
            "let"

        Const ->
            "const"


produceDeclarators : List Declarator -> Doc
produceDeclarators declarators =
    declarators
        |> List.map produceDeclarator
        |> Doc.join (Doc.char ',')


produceDeclarator : Declarator -> Doc
produceDeclarator (Declarator identifier expression) =
    produceIdentifier identifier
        |+ Doc.char '='
        |+ produceExpression expression


produceArguments : List Identifier -> Doc
produceArguments arguments =
    arguments
        |> List.map produceIdentifier
        |> Doc.join (Doc.char ',')
        |> Doc.parens


produceExpression : Expression -> Doc
produceExpression expression =
    Debug.crash "TO DO"


produceIdentifier : Identifier -> Doc
produceIdentifier =
    Doc.string


type Statement
    = StatementList (List Statement)
    | BlockStatement (List Statement)
    | ExpressionStatement Expression
    | IfStatement Expression Statement Statement
    | VariableDeclaration DeclarationKind (List Declarator)
    | FunctionDeclaration (List Identifier) Statement
    | NamedFunctionDeclaration Identifier (List Identifier) Statement


type DeclarationKind
    = Var
    | Let
    | Const


type Declarator
    = Declarator Identifier Expression


type alias Identifier =
    String


type Expression
    = Identifier Identifier
    | Literal Literal
    | ArrayExpression (List Expression)
    | ObjectExpression (List Property)
    | MemberExpression Expression Expression
    | CallExpression Expression Expression (List Expression)
      --| GroupingOperator Expression
      --| UnaryExpression UnaryOperator Expression
      --| BinaryExpression BinaryOperator Expression Expression
      --| ConditionalExpression Expression Expression Expression
    | AssignmentExpression Expression Expression
    | ArrowFunction (List Identifier) Statement
    | ConciseArrowFunction (List Identifier) Expression


type Literal
    = NullLiteral
    | BoolLiteral Bool
    | IntLiteral Int
    | FloatLiteral Float
    | StringLiteral String
    | RegexLiteral String


type Property
    = Property Expression Expression



--type UnaryOperator
--    = New
--    | Delete
--    | Void
--    | TypeOf
--    | PrefixIncrement
--    | PrefixDecrement
--    | PostfixIncrement
--    | PostfixDecrement
--    | UnaryPlus
--    | UnaryMinus
--    | BitwiseNot
--    | LogicalNot
--type BinaryOperator
--    = Times
--    | DividedBy
--    | Mod
--    | Plus
--    | Minus
--    | Incr
--    | LeftShift
--    | RightShift
--    | UnsignedRightShift
--    | LessThan
--    | GreaterThan
--    | LessThanOrEqual
--    | InstanceOf
--    | In
--    | GreaterThanOrEqual
--    | Equals
--    | DoesNotEqual
--    | StrictEquals
--    | StrictDoesNotEqual
--    | BitwiseAnd
--    | BitwiseOr
--    | BitwiseXOr
--    | And
--    | Or
