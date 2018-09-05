module Language.JavaScript.Ast exposing (..)


type Program
    = Module (List ModuleItem)
    | Script StatementList



-- Module


type ModuleItem
    = ImportDeclaration ImportDeclaration
    | ExportDeclaration ExportDeclaration
    | ModuleStatementListItem StatementListItem


type ImportDeclaration
    = Import ImportClause ModuleSpecifier
    | ImportModuleSpecifier ModuleSpecifier


type ImportClause
    = NameSpaceImport (Maybe Identifier) Identifier
    | NamedImport (Maybe Identifier) (List ImportSpecifier)


type ImportSpecifier
    = ImportSpecifier Identifier
    | AsImportSpecifier Identifier Identifier


type ModuleSpecifier
    = ModuleSpecifier StringLiteral


type ExportDeclaration
    = ExportAllFrom ModuleSpecifier
    | ExportFrom (List ExportSpecifier) ModuleSpecifier
    | DeclarationExport Declaration
    | DefaultDeclarationExport Declaration
    | ExpressionExport Expression
    | DefaultExpressionExport Expression


type ExportSpecifier
    = ExportSpecifier Identifier
    | AsExportSpecifier Identifier Identifier



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


type Block
    = Block StatementList


type StatementList
    = StatementList (List StatementListItem)


type StatementListItem
    = StatementItem Statement
    | DeclarationItem Declaration


type Declaration
    = Const (List Binding)
    | Let (List Binding)
    | Var (List Binding)
    | FunctionDeclaration Identifier Parameters Block
    | GeneratorDeclaration Identifier Parameters Block
    | AsyncFunctionDeclaration Identifier Parameters Block
    | AsyncGeneratorDeclaration Identifier Parameters Block
    | ClassDeclaration Identifier Heritage (List ClassElement)


type Binding
    = IdentifierBinding Identifier (Maybe Expression)
    | PatternBinding Pattern Expression


type alias Identifier =
    String


type Pattern
    = ObjectPattern (List BindingProperty) (Maybe Identifier)
    | ArrayPattern (List BindingElement) (Maybe Identifier)


type BindingProperty
    = SingleNameBindingProperty Identifier (Maybe Expression)
    | ExpandedBindingProperty PropertyName BindingElement


type Parameters
    = Parameters (List BindingElement) (Maybe BindingRestElement)


type BindingElement
    = SingleNameBindingElement Identifier (Maybe Expression)
    | PatternElement Pattern (Maybe Expression)


type BindingRestElement
    = BindingRestElementIdentifier Identifier
    | BindingRestElementPattern Pattern


type IterationStatement
    = DoWhile Statement Expression
    | While Expression Statement
    | For ForDeclaration (Maybe Expression) (Maybe Expression) Statement
    | ForIn ForBinding Expression Statement
    | ForOf ForBinding Expression Statement


type ForDeclaration
    = ForConst (List Binding)
    | ForLet (List Binding)
    | ForVar (List Binding)


type ForBinding
    = ForIdentifier Identifier
    | ForPattern Pattern


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
    | TryParameterPattern Pattern


type MethodDefinition
    = NormalMethod PropertyName Parameters Block
    | GeneratorMethod PropertyName Parameters Block
    | AsyncMethod PropertyName Parameters Block
    | AsyncGeneratorMethod PropertyName Parameters Block
    | Get PropertyName Block
    | Set PropertyName Parameters Block


type ClassElement
    = ClassMethod MethodDefinition
    | StaticClassMethod MethodDefinition



-- Expression


type Expression
    = This
    | IdentifierExpression Identifier
    | LiteralExpression Literal
    | ObjectLiteral (List PropertyDefinition)
    | ArrayLiteral (List Expression) (Maybe Expression)
    | TemplateLiteral (List TemplateItem)
    | UnaryExpression UnaryOperator Expression
    | UpdateExpression UpdateOperator LeftHandSideExpression
    | BinaryExpression Expression BinaryOperator Expression
    | ConditionalExpression Expression Expression Expression
    | GroupingExpression Expression
    | AssignmentExpression LeftHandSideExpression AssignmentOperator Expression
    | FunctionExpression FunctionExpression
    | MemberExpression MemberExpression
    | CallExpression Callee Arguments
    | NewExpression Identifier Arguments
    | Await Expression
    | Yield Expression
    | YieldGenerator Expression


type UnaryOperator
    = Delete
    | Void
    | TypeOf
    | UnaryPlus
    | UnaryNegation
    | BitwiseNot
    | LogicalNot


type UpdateOperator
    = PostfixIncrement
    | PostfixDecrement
    | PrefixIncrement
    | PrefixDecrement


type BinaryOperator
    = -- Arithmetic Operators
      Addition
    | Subtraction
    | Multiplication
    | Division
      -- Bitwise Operators
    | BitwiseAND
    | BitwiseXOR
    | BitwiseOR
    | BitwiseNOT
    | LeftShift
    | RightShift
    | UnsignedRightShift
      -- Comparison Operators
    | Equality
    | Inequality
    | StrictEquality
    | StrictInequality
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    | InstanceOf
    | In


type AssignmentOperator
    = Assignment
    | AdditionAssignment
    | SubtractionAssignment
    | MultiplicationAssignment
    | DivisionAssignment
    | RemainderAssignment
    | ExponentiationAssignment
    | LeftShiftAssignment
    | RightShiftAssignment
    | UnsignedRightShiftAssignment
    | BitwiseANDAssignment
    | BitwiseXORAssignment
    | BitwiseORAssignment


type LeftHandSideExpression
    = LeftHandSideIdentifier Identifier
    | LeftHandSideMemberExpression MemberExpression
    | LeftHandSidePattern Pattern
    | LeftHandSideSuperProperty Property
    | LeftHandSideNewTarget


type Literal
    = NullLiteral
    | BooleanLiteral Bool
    | Numeric Float
    | StringLiteralExpression StringLiteral


type StringLiteral
    = StringLiteral String


type PropertyDefinition
    = KeyValueProperty PropertyName Expression
    | MethodProperty MethodDefinition
    | SpreadProperty Expression
    | ShortHandProperty Identifier


type PropertyName
    = IdentifierProperty Identifier
    | StringProperty StringLiteral
    | NumericProperty Float
    | ComputedPropertyName Expression


type TemplateItem
    = TemplateString String
    | TemplateSubstitution Expression


type MemberExpression
    = Member Expression Property


type Property
    = ComputedProperty Expression
    | DotProperty Identifier


type FunctionExpression
    = NormalFunctionExpression (Maybe Identifier) Parameters Block
    | ArrowFunctionExpression ArrowFunctionParameters ArrowFunctionBody
    | AsyncFunctionExpression (Maybe Identifier) Parameters Block
    | GeneratorExpression (Maybe Identifier) Parameters Block
    | AsyncGeneratorExpression (Maybe Identifier) Parameters Block
    | ClassExpression (Maybe Identifier) Heritage (List MethodDefinition)


type ArrowFunctionParameters
    = UnparenthesizedArrowFunctionParam Identifier
    | NormalArrowFunctionParams Parameters


type ArrowFunctionBody
    = ConciseArrowFunctionBody Expression
    | NormalArrowFunctionBody Block


type Heritage
    = NoHeritage
    | NullHeritage
    | Heritage Identifier


type Callee
    = IdentifierCallee Identifier
    | MemberExpressionCallee MemberExpression
    | FunctionExpressionCallee FunctionExpression
    | SuperCallee


type Arguments
    = Arguments (List Expression) (Maybe Identifier)
