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

        --VariableDeclaration kind declarators ->
        --    produceDeclarationKind kind
        --        |+ Doc.space
        --        |+ produceDeclarators declarators
        --        |+ Doc.char ';'
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



--produceDeclarationKind : DeclarationKind -> Doc
--produceDeclarationKind =
--    declarationKindToString >> Doc.string
--declarationKindToString : DeclarationKind -> String
--declarationKindToString kind =
--    case kind of
--        Var ->
--            "var"
--        Let ->
--            "let"
--        Const ->
--            "const"
--produceDeclarators : List Declarator -> Doc
--produceDeclarators declarators =
--    declarators
--        |> List.map produceDeclarator
--        |> Doc.join (Doc.char ',')
--produceDeclarator : Declarator -> Doc
--produceDeclarator (Declarator identifier expression) =
--    produceIdentifier identifier
--        |+ Doc.char '='
--        |+ produceExpression expression


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



-- Statement


type Statement
    = BlockStatement Block
    | Empty
    | ExpressionStatement Expression
    | IterationStatement IterationStatement
    | IfStatement Expression Statement (Maybe Statement)
    | Return (Maybe Expression)
    | Break (Maybe Identifier)
    | Continue (Maybe Identifier)
    | Throw Expression
    | Switch Expression (List CaseClause) (Maybe DefaultClause)
    | TryStatement TryStatement
    | Label Identifier Statement
    | Debugger
      -- TODO
    | ImportStatement
    | ImportMeta
    | ExportStatement


type Block
    = Block StatementList


type StatementList
    = StatementList (List StatementListItem)


type StatementListItem
    = StatementItem Statement
    | DeclarationItem Declaration


type Declaration
    = Const Binding
    | Let Binding
    | Var Binding
    | FunctionDeclaration Identifier Parameters (List Statement)
    | GeneratorDeclaration Identifier Parameters (List Statement)
    | AsyncFunctionDeclaration Identifier Parameters (List Statement)
    | AsyncGeneratorDeclaration Identifier Parameters (List Statement)
    | ClassDeclaration Identifier (Maybe LeftHandSideExpression) (List MethodDefinition)


type Binding
    = BindingIdentifier Identifier (Maybe AssignmentExpression)
    | BindingPattern BindingPattern AssignmentExpression


type BindingPattern
    = -- TODO
      ObjectBindingPattern
    | ArrayBindingPattern


type Parameters
    = Parameters (List BindingElement) (Maybe BindingRestElement)


type BindingElement
    = SingleNameBindingElement Identifier (Maybe AssignmentExpression)
    | BindingPatternElement BindingPattern (Maybe AssignmentExpression)


type BindingRestElement
    = BindingRestElementIdentifier Identifier
    | BindingRestElementPattern BindingPattern


type IterationStatement
    = DoWhile Statement Expression
    | While Expression Statement
    | For (List Declaration) (Maybe Expression) (Maybe Expression) Statement
    | ForIn ForBinding Expression Statement
    | ForOf ForBinding Expression Statement


type ForBinding
    = ForBindingIdentifier Identifier
    | ForBindingPattern BindingPattern


type CaseClause
    = CaseClause Expression StatementList


type DefaultClause
    = DefaultClause StatementList


type TryStatement
    = TryCatch Block TryParameter Block
    | TryFinally Block Block
    | TryCatchFinally Block TryParameter Block Block


type TryParameter
    = TryParameterIdentifier Identifier
    | TryParameterPattern BindingPattern



--Expression


type Expression
    = AssignmentExpression AssignmentExpression
    | CommaExpressions (List AssignmentExpression)


type AssignmentExpression
    = ArithmeticOperator
    | ArrayComprehensions
    | AssignmentOperators
    | BitwiseOperators
    | CommaOperator
    | ComparisonOperators
    | ConditionalOperator
    | DesetructuringAssignemnts
    | ExpressionClosures
    | GeneratorComprehensions
    | GroupingOperator
    | LegacyGeneratorFunctionExpression
    | LogicalOperators
    | ObjectInitializer
    | OperatorPrecedence
    | PipelineOperator
    | PropertyAccessors
    | SpreadSyntax
    | AsyncFunctionExpression
    | Await
    | ClassExpression
    | DeleteOperator
    | FunctionExpression
    | GeneratorExpression
    | InOperator
    | InstanceOf
    | NewOperator
    | NewTarget
    | Super
    | This
    | TypeOf
    | VoidOperator
    | Yield
    | YieldGenerator


type Identifier
    = Identifier String


type LeftHandSideExpression
    = LeftHandSideExpression


type MethodDefinition
    = MethodDefinition



-- Expressions
--type Expression
--    = IdentifierReference IdentifierReference
--      --| BindingIdentifier BindingIdentifier
--    | Identifier IdentifierName
--      --| AsyncArrowBindingIdentifier AsyncArrowBindingIdentifier
--      --| LabelIdentifier LabelIdentifier
--    | PrimaryExpression PrimaryExpression
--    | CoveredParenthesizedExpressionAndArrowParameterList CoveredParenthesizedExpressionAndArrowParameterList
--type IdentifierReference
--    = IdentifierReferenceIdentifier IdentifierName
--    | IdentifierReferenceYield
--    | IdentifierReferenceAwait
--type BindingIdentifier
--    = BindingIdentifierIdentifier IdentifierName
--    | BindingIdentifierYield
--    | BindingIdentifierAwait
--type PrimaryExpression
--    = This
--    | PrimaryExpressionIdentifierReference IdentifierReference
--    | Literal Literal
--    | ArrayLiteral (List AssignmentExpression) (Maybe AssignmentExpression)
--    | ObjectLiteral (List PropertyDefinition)
--    | FunctionExpression FunctionExpression
--    | ClassExpression ClassExpression
--      --| GeneratorExpression GeneratorExpression
--      --| AsyncFunctionExpression AsyncFunctionExpression
--      --| AsyncGeneratorExpression AsyncGeneratorExpression
--      --| RegularExpressionLiteral RegularExpressionLiteral
--      --| TemplateLiteral TemplateLiteral
--    | PrimaryExpressionCoveredParenthesizedExpressionAndArrowParameterList CoveredParenthesizedExpressionAndArrowParameterList
--type CoveredParenthesizedExpressionAndArrowParameterList
--    = ExpressionInParenthesis Expression
--    | ExpressionInParenthesisTrailingComma Expression
--    | EmptyParenthesis
--    | ElipsisBindingIdentifier BindingIdentifier
--    | ElipsisBindingPattern BindingPattern
--    | ExpressionElipsisBindingIdentifier Expression BindingIdentifier
--    | ExpressionElipsisBindingPattern Expression BindingPattern
--type Literal
--    = NullLiteral
--    | BooleanLiteral Bool
--    | NumericLiteral Float
--    | StringLiteral String
--type PropertyDefinition
--    = IdentifierReferencePropertyDefinition IdentifierReference
--      --| CoverInitializedName CoverInitializedName
--    | KeyValue PropertyName AssignmentExpression
--    | MethodDefinition MethodDefinition
--    | SpreadProperties AssignmentExpression
--type PropertyName
--    = LiteralPropertyName LiteralPropertyName
--    | ComputedPropertyName AssignmentExpression
--type LiteralPropertyName
--    = IdentifierNameProperty Identifier
--    | StringLIteralProperty String
--    | NumericLiteralProperty Float
--type MemberExpression
--    = PrimaryMemberExpression PrimaryExpression
---- Statements
----type Statement
----    = BlockStatement (List StatementListItem)
----    | VariableStatement (List Binding)
----    | EmptyStatement
----    | ExpressionStatement Expression
----    | IfStatement IfStatement
----    | BreakableStatement BreakableStatement
----      --| ContinueStatement
----    | BreakStatement
----    | ReturnStatement (Maybe Expression)
------| WithStatement
------| LabelledStatement
------| ThrowStatement
------| TryStatement
------| DebuggerStatement
----type Declaration
----    = HoistableDeclaration HoistableDeclaration
----    | ClassDeclaration
----    | LexicalDeclaration LexicalDeclaration
----type HoistableDeclaration
----    = FunctionDeclaration
------| GeneratorDeclaration
------| AsyncFunctionDeclaration
------| AsyncGeneratorDeclaration
----type BreakableStatement
----    = IterationStatement IterationStatement
----    | SwitchStatement Expression CaseBlock
----type StatementListItem
----    = StatementLIstStatement Statement
----    | StatementListDeclaration Declaration
----type LexicalDeclaration
----    = Let (List Binding)
----    | Const (List Binding)
----type Binding
----    = BindingIdentifier BindingIdentifier (Maybe AssignmentExpression)
----    | BindingPattern BindingPattern AssignmentExpression
----type BindingPattern
----    = ObjectBindingPattern
----    | ArrayBindingPattern
----type IfStatement
----    = IfElse Expression Statement Statement
----    | If Expression Statement
----type CaseBlock
----    = CaseNoDefault (List CaseClause)
----    | CaseDefault (List CaseClause) DefaultClause (List CaseClause)
----type CaseClause
----    = CaseClause Expression (List Statement)
----type DefaultClause
----    = Default (List Statement)
---- Complete when necessary
----type IterationStatement
----    = DoWhile Statement Expression
----    | While Expression Statement
----    | For
----    | ForIn
----    | ForOf
-------------------------
------type Statement
------    = StatementList (List Statement)
------    | BlockStatement (List Statement)
------    | ExpressionStatement Expression
------    | IfStatement Expression Statement Statement
------    | VariableDeclaration DeclarationKind (List Declarator)
------    | FunctionDeclaration Identifier (List Identifier) Statement
----type Declarator
----    = Declarator Identifier Expression
----type alias Identifier =
----    String
----type Expression
----    = This
----    | GroupingOperator Expression
----    | Identifier Identifier
----    | Literal Literal
----    | ArrayExpression (List Expression)
----    | ObjectExpression (List Property)
----    | MemberExpression Expression Expression
----    | CallExpression Expression (List Expression)
----    | NewExpression Identifier (List Expression)
----    | AssignmentExpression AssignmentOperator Expression Expression
----    | FunctionExpression (List Identifier) Statement
----    | ArrowFunction (List Identifier) Statement
----    | ConciseArrowFunction (List Identifier) Expression
----    | UnaryExpression UnaryOperator Expression
----    | BinaryExpression BinaryOperator Expression Expression
----    | TernaryExpression Expression Expression Expression
----    | CommaOperator (List Expression)
--type Property
--    = Property Expression Expression
--type UnaryOperator
--    = Delete
--    | Void
--    | TypeOf
--    | UnaryPlus
--    | UnaryMinus
--    | BitwiseNot
--    | LogicalNot
--    | PrefixIncrement
--    | PrefixDecrement
--    | PostfixIncrement
--    | PostfixDecrement
--type BinaryOperator
--    = ArithmeticOperator ArithmeticOperator
--    | RelationalOperator RelationalOperator
--    | EqualityOperator EqualityOperator
--    | BitwiseShiftOperator BitwiseShiftOperator
--    | BinaryBitwiseOperator BinaryBitwiseOperator
--    | BinaryLogicalOperator BinaryLogicalOperator
--type AssignmentOperator
--    = Assignment
--    | ArithmeticAssignment ArithmeticOperator
--    | BitwiseShiftAssignment BitwiseShiftOperator
--    | BinaryBitwiseAssignment BinaryBitwiseOperator
--type ArithmeticOperator
--    = Addition
--    | Subtraction
--    | Division
--    | Multiplication
--    | Remainder
--    | Exponentiation
--type RelationalOperator
--    = In
--    | InstanceOf
--    | LessThan
--    | GreaterThan
--    | LessThanOrEqual
--    | GreaterThanOrEqual
--type EqualityOperator
--    = Equality
--    | Inequality
--    | StrictEquality
--    | StrictInequality
--type BitwiseShiftOperator
--    = LeftShift
--    | RightShift
--    | UnsignedRightShift
--type BinaryBitwiseOperator
--    = BitwiseAnd
--    | BitwiseXOr
--    | BitwiseOr
--type BinaryLogicalOperator
--    = LogicalAnd
--    | LogicalOr
---- Comma operator?
