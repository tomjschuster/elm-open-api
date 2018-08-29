module Language.JavaScript.Ast exposing (..)

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

        FunctionDeclaration name params body ->
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
    | FunctionDeclaration Identifier (List Identifier) Statement


type DeclarationKind
    = Var
    | Let
    | Const


type Declarator
    = Declarator Identifier Expression


type alias Identifier =
    String


type Expression
    = This
    | GroupingOperator Expression
    | Identifier Identifier
    | Literal Literal
    | ArrayExpression (List Expression)
    | ObjectExpression (List Property)
    | MemberExpression Expression Expression
    | CallExpression Expression (List Expression)
    | NewExpression Identifier (List Expression)
    | AssignmentExpression AssignmentOperator Expression Expression
    | FunctionExpression (List Identifier) Statement
    | ArrowFunction (List Identifier) Statement
    | ConciseArrowFunction (List Identifier) Expression
    | UnaryExpression UnaryOperator Expression
    | BinaryExpression BinaryOperator Expression Expression
    | TernaryExpression Expression Expression Expression
    | CommaOperator (List Expression)


type Literal
    = NullLiteral
    | BoolLiteral Bool
    | IntLiteral Int
    | FloatLiteral Float
    | StringLiteral String
    | RegexLiteral String


type Property
    = Property Expression Expression


type UnaryOperator
    = Delete
    | Void
    | TypeOf
    | UnaryPlus
    | UnaryMinus
    | BitwiseNot
    | LogicalNot
    | PrefixIncrement
    | PrefixDecrement
    | PostfixIncrement
    | PostfixDecrement


type BinaryOperator
    = ArithmeticOperator ArithmeticOperator
    | RelationalOperator RelationalOperator
    | EqualityOperator EqualityOperator
    | BitwiseShiftOperator BitwiseShiftOperator
    | BinaryBitwiseOperator BinaryBitwiseOperator
    | BinaryLogicalOperator BinaryLogicalOperator


type AssignmentOperator
    = Assignment
    | ArithmeticAssignment ArithmeticOperator
    | BitwiseShiftAssignment BitwiseShiftOperator
    | BinaryBitwiseAssignment BinaryBitwiseOperator


type ArithmeticOperator
    = Addition
    | Subtraction
    | Division
    | Multiplication
    | Remainder
    | Exponentiation


type RelationalOperator
    = In
    | InstanceOf
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual


type EqualityOperator
    = Equality
    | Inequality
    | StrictEquality
    | StrictInequality


type BitwiseShiftOperator
    = LeftShift
    | RightShift
    | UnsignedRightShift


type BinaryBitwiseOperator
    = BitwiseAnd
    | BitwiseXOr
    | BitwiseOr


type BinaryLogicalOperator
    = LogicalAnd
    | LogicalOr



-- Comma operator?
