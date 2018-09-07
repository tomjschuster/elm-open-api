module Language.JavaScript.Ast exposing (..)

-- Program


type Program
    = Module (List ModuleItem)
    | Script (List StatementListItem)



-- Module


type ModuleItem
    = ImportItem Import
    | ExportItem Export
    | ModuleStatementListItem StatementListItem


type Import
    = Import ImportClause ModulePath
    | ImportModulePath ModulePath


type ImportClause
    = NameSpaceImport (Maybe Identifier) Identifier
    | NamedImport (Maybe Identifier) (List ImportSpecifier)


type ImportSpecifier
    = ImportSpecifier Identifier
    | AsImportSpecifier Identifier Identifier


type alias ModulePath =
    String


type Export
    = ExportAllFrom ModulePath
    | ExportFrom (List ExportSpecifier) ModulePath
    | VariableStatementExport (List Binding)
    | DeclarationExport Declaration
    | DefaultHoistableDeclarationExport HoistableDeclaration
    | DefaultExpressionExport Expression
    | DefaultClassDeclarationExport Identifier Heritage (List ClassElement)


type ExportSpecifier
    = ExportSpecifier Identifier
    | AsExportSpecifier Identifier Identifier



-- Statement


type Statement
    = BlockStatement Block
    | Empty
    | VariableStatement (List Binding)
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
    = Block (List StatementListItem)


type StatementListItem
    = StatementItem Statement
    | DeclarationItem Declaration


type Declaration
    = LexicalDeclaration LexicalDeclaration
    | HoistableDeclaration HoistableDeclaration
    | ClassDeclaration Identifier Heritage (List ClassElement)


type LexicalDeclaration
    = Const (List Binding)
    | Let (List Binding)


type HoistableDeclaration
    = FunctionDeclaration Identifier Parameters Block
    | GeneratorDeclaration Identifier Parameters Block
    | AsyncFunctionDeclaration Identifier Parameters Block
    | AsyncGeneratorDeclaration Identifier Parameters Block


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
    | For Declarator (List Binding) (Maybe Expression) (Maybe Expression) Statement
    | ForIn Declarator Binding Expression Statement
    | ForOf Declarator Binding Expression Statement


type Declarator
    = ConstDeclarator
    | LetDeclarator
    | VarDeclarator


type ForBinding
    = ForIdentifier Identifier
    | ForPattern Pattern


type alias CaseClause =
    ( Expression, List StatementListItem )


type alias DefaultClause =
    List StatementListItem


type TryStatement
    = TryCatch Block TryParameter Block
    | TryFinally Block Block
    | TryCatchFinally Block TryParameter Block Block


type TryParameter
    = TryIdentifier Identifier
    | TryPattern Pattern


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
    | ArrayLiteral (List ArrayElement)
    | TemplateLiteral (List TemplateItem)
    | UnaryExpression UnaryOperator Expression
    | UpdateExpression OperatorPosition UpdateOperator LeftHandSideExpression
    | BinaryExpression Expression BinaryOperator Expression
    | ConditionalExpression Expression Expression Expression
    | GroupingExpression Expression
    | AssignmentExpression LeftHandSideExpression AssignmentOperator Expression
    | FunctionExpression FunctionExpression
    | MemberExpression MemberExpression
    | CallExpression Callee Arguments
    | NewExpression Identifier (Maybe Arguments)
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
    = Increment
    | Decrement


type OperatorPosition
    = Prefix
    | Postfix


type BinaryOperator
    = -- Arithmetic Operators
      Addition
    | Subtraction
    | Multiplication
    | Division
    | Exponentiation
      -- Bitwise Operators
    | BitwiseAND
    | BitwiseXOR
    | BitwiseOR
    | LeftShift
    | RightShift
    | UnsignedRightShift
      -- Logical Operators
    | And
    | Or
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


type ArrayElement
    = ExpressionElement Expression
    | SpreadElement Identifier


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
    | ClassExpression (Maybe Identifier) Heritage (List ClassElement)


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
    = Arguments (List Expression) (Maybe Expression)
