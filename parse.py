from logging.config import IDENTIFIER
from mimetypes import add_type
from turtledemo.penrose import inflatedart

import ASTNodeDefs as AST


class Lexer:
    def __init__(self, code):
        self.code = code
        self.position = 0
        self.current_char = self.code[self.position]
        self.tokens = []

    # Move to the next position in the code increment by one.
    def advance(self):
        self.position += 1
        if self.position >= len(self.code):
            self.current_char = None
        else:
            self.current_char = self.code[self.position]

    # If the current char is whitespace, move ahead.
    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # Tokenize the identifier.
    def identifier(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return ('IDENTIFIER', result)

    # Tokenize numbers, including float handling
    def number(self):
        result = ''
        is_float=False
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if is_float:
                    raise ValueError(f"invalid number format {self.position}")
                is_float=True
            result += self.current_char
            self.advance()
        # TODO: Update this code to handle floating-point numbers

        if is_float:
            return ('FNUMBER', float(result))
        else:
            return ('NUMBER', int(result))

    def token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char.isalpha():
                ident = self.identifier()
                if ident[1] == 'if':
                    return ('IF', 'if')
                elif ident[1] == 'else':
                    return ('ELSE', 'else')
                elif ident[1] == 'while':
                    return ('WHILE', 'while')
                elif ident[1] == 'int':
                    return ('INT', 'int')
                elif ident[1] == 'float':
                    return ('FLOAT', 'float')
                return ident  # Generic identifier
            if self.current_char.isdigit() or self.current_char == '.':
                return self.number()
            if self.current_char == '+':
                self.advance()
                return ('PLUS', '+')
            if self.current_char == '-':
                self.advance()
                return ('MINUS', '-')
            if self.current_char == '*':
                self.advance()
                return ('MULTIPLY', '*')
            if self.current_char == '/':
                self.advance()
                return ('DIVIDE', '/')
            if self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return ('EQ', '==')
                return ('EQUALS', '=')
            if self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return ('NEQ', '!=')
            if self.current_char == '<':
                self.advance()
                return ('LESS', '<')
            if self.current_char == '>':
                self.advance()
                return ('GREATER', '>')
            if self.current_char == '(':
                self.advance()
                return ('LPAREN', '(')
            if self.current_char == ')':
                self.advance()
                return ('RPAREN', ')')
            if self.current_char == ',':
                self.advance()
                return ('COMMA', ',')
            if self.current_char == ':':
                self.advance()
                return ('COLON', ':')
            # TODO: Implement handling for '{' and '}' tokens here (see `tokens.txt` for exact names)
            if self.current_char == '{':
                self.advance()
                return ('LBRACE', '{')  # WHAT IS IT CALLED
            if self.current_char == '}':
                self.advance()
                return ('RBRACE', '}')  # WHAt is it called
            if self.current_char == '\n':
                self.advance()
                continue

            raise ValueError(f"Illegal character at position {self.position}: {self.current_char}")

        return ('EOF', None)

    # Collect all the tokens in a list.
    def tokenize(self):
        while True:
            token = self.token()
            self.tokens.append(token)
            if token[0] == 'EOF':
                break
        return self.tokens


import ASTNodeDefs as AST


class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_token = tokens.pop(0)
        # Use these to track the variables and their scope
        self.symbol_table = {'global': {}}
        self.scope_counter = 0
        self.scope_stack = ['global']
        self.messages = []

    def error(self, message):
        self.messages.append(message)

    def advance(self):
        if self.tokens:
            self.current_token = self.tokens.pop(0)

    # TODO: Implement logic to enter a new scope, add it to symbol table, and update `scope_stack`
    def enter_scope(self):
        self.scope_counter += 1
        newscope_name = f'scope{self.scope_counter}'

        if newscope_name not in self.symbol_table: #this is added, delet later
            self.symbol_table[newscope_name] = {}
        self.scope_stack.append(newscope_name)

       # self.symbol_table[newscope_name]= {}  #dictionary
       # self.scope_stack.append(newscope_name)


        #create new dictionary entry in self.symbol_table w/ a new name
        #add it to self.scope_stack
        #update self.scope_counter


    # TODO: Implement logic to exit the current scope, removing it from `scope_stack`
    def exit_scope(self):
        if self.scope_stack:  #thisis added

            self.scope_stack.pop()

       # s=self.scope_stack[-1]# added that fr
       # self.symbol_table.pop(s)#added yhat fr
       # self.scope_stack.pop()





        #remove from scope stack
        #pop from dictionary


    # Return the current scope name
    def current_scope(self):
        return self.scope_stack[-1]

    # TODO: Check if a variable is already declared in the current scope; if so, log an error
    def checkVarDeclared(self, identifier):

        # compare identifier w/ entries in the dictionary
        current_scope_name=self.current_scope()# come from
        current_scope= self.symbol_table[current_scope_name]  #come from
        if identifier in  self.symbol_table[self.current_scope()] :  #global
            self.error(f"Variable {identifier} has already been declared in the current scope")

    # TODO: Check if a variable is declared in any accessible scope; if not, log an error
    def checkVarUse(self, identifier):
        # loop backwards in the stack and see if identifier has been declared
        if not any(identifier in self.symbol_table[scope] for scope in reversed(self.scope_stack)):

        #if not  identifier in self.symbol_table: #unsure do reverse

            self.error(f"Variable {identifier} has not been declared in the current or any enclosing scopes")

    # TODO: Check type mismatch between two entities; log an error if they do not match
    def checkTypeMatch2(self, vType, eType, var, exp):
        if vType == "int"  and eType == "float" : #or (vType ==" float"  and eType == "int"):
            self.error(f"Type Mismatch between {vType} and {eType}")
        elif vType== "float" and eType == "int":
            self.error(f"Type Mismatch between {vType} and {eType}")

        #reversed in var #still thinking


        # loop backwards again in the stack and compare if vType and eType
         #if either not work, ignore
         # not in eType


    # TODO: Implement logic to add a variable to the current scope in `symbol_table`
    def add_variable(self, name, var_type):
        if name not in self.symbol_table[self.current_scope()]:
            self.symbol_table[self.current_scope()][name] = var_type

        #self.checkVarDeclared(name)


      # add back, if self.checkVarDeclared(name) not in self.symbol_table[self.current_scope()]:   #come back
          #  self.symbol_table[self.current_scope()][name] = var_type
           # self.symbol_table[self.current_scope()].update({name:var_type})


        #check Variable, as argument, add the name in the curretn scope in the symbol table
        #if self.current_token(name,var_type) in self.symbol_table[name]:
            #var_type()
        #how to add, is their a function.
        # how to add a variable is to call dictionary name "dictionary"[name] = type



    # TODO: Retrieve the variable type from `symbol_table` if it exists
    def get_variable_type(self, name):
        for x in reversed(self.scope_stack):
            if name in self.symbol_table[x]:
                return self.symbol_table[x][name]


                   #{name: type}
            # return dictionary[scope][name]revers, nested loop
           # var_type()
          #  if not any(identifier in self.symbol_table[scope] for scope in reversed(self.scope_stack)):, use this

        # similar to add_variable
        return None

    def parse(self):
        return self.program()

    def program(self):
        statements = []
        while self.current_token[0] != 'EOF':
            statements.append(self.statement())
        return AST.Block(statements)

    # TODO: Modify the `statement` function to dispatch to declare statement
    def statement(self):
     #   if self.current_token[0] in ["INT","FLOAT"]:
         #   return self.decl_stmt()
        if self.current_token[0] == 'IDENTIFIER':
            if self.peek() == 'EQUALS':
                return self.assign_stmt()
            elif self.peek() == 'LPAREN':
                return self.function_call()
            else:
                raise ValueError(f"Unexpected token after identifier: {self.current_token}")
        elif self.current_token[0] == 'IF':
            return self.if_stmt()
        elif self.current_token[0] == 'WHILE':
            return self.while_stmt()
        elif self.current_token[0]=="INT" or self.current_token[0]=="FLOAT": #added this from
            return self.decl_stmt()
        else:
            raise ValueError(f"Unexpected token: {self.current_token}")

    # TODO: Implement the declaration statement and handle adding the variable to the symbol table
    def decl_stmt(self):

        vt,var_type = self.current_token#[1] added vt
        if vt not in ['INT','FLOAT']:
            raise ValueError()  #gto from raise ValueError()
        self.advance()
        vt,var_name = self.current_token#[1]
        self.advance()
        self.checkVarDeclared(var_name)
        self.expect('EQUALS')
        expression = self.expression()

        #expr_type = expression.value_type
       # self.expect('INT','FLOAT')
        self.checkTypeMatch2(var_type,expression.value_type,var_name,expression)

        self.add_variable(var_name, var_type)

        """
        Parses a declaration statement.
        Example:
        int x = 5
        float y = 3.5
        TODO: Implement logic to parse type, identifier, and initialization expression and also handle type checking
        """
        #Check for INT and FLOAT and the rest should be similar to assign statement,checktypematch,add symbol table
        return AST.Declaration(var_type, var_name, expression)
    # TODO: Parse assignment statements, handle type checking
    def assign_stmt(self):
        #identifier= self.current_token[1]
        #self.checkVarUse(identifier)
        #var_type= self.get_variable_type(identifier)
        #self.advance()
        #self.expect('EQUALS')
        #expr_type=None
        #expression = self.expression'
       # identifier= self.current_token[1]
        var_name = self.current_token[1] #might remove
        self.checkVarUse(var_name)
        var_type = self.get_variable_type(var_name)
        self.advance()
        self.expect('EQUALS')
        expression = self.expression()
        expression_type= expression.value_type
        self.checkTypeMatch2(self.get_variable_type(var_name),expression.value_type,var_name,expression)
        """
        Parses an assignment statement.
        Example:
        x = 10ng.
        """
        return AST.Assignment(var_name, expression)

    # TODO: Implement the logic to parse the if condition and blocks of code
    def if_stmt(self):
       # self.advance()
        self.expect('IF')
        #TODO: Implement logic to handle assignment, including type check
        condition = self.boolean_expression()
        self.expect('LBRACE')
        self.enter_scope()
        then_block = self.block()
      #  block = self.block()
        self.exit_scope()
        else_block=None
        self.expect('RBRACE')
        if self.current_token[0] == 'ELSE':
            self.advance()
            self.expect('LBRACE')
            self.enter_scope()
          #  else_block=self.block()
            #self.expect('COLON')
      #  else:

            else_block = self.block()
            self.exit_scope()
            self.expect('RBRACE')


        """
        Parses an if-statement, with an optional else block.
        Example:
        if condition {
            # statements
        }
        else {
            # statements
        }
        TODO: Implement the logic to parse the if condition and blocks of code.
        """
        return AST.IfStatement(condition, then_block, else_block)

    # TODO: Implement the logic to parse while loops with a condition and a block of statements
    def while_stmt(self):
        self.expect('WHILE')
        #self.advance()
        condition = self.boolean_expression()
        # self.expect('WHILE')
        self.expect('LBRACE')
        self.enter_scope()
        block = self.block()
        self.exit_scope()
        self.expect('RBRACE')

        #same as last
        """
        Parses a while-statement.
        Example:
        while condition {
            # statements
        }
        TODO: Implement the logic to parse while loops with a condition and a block of statements.
        """
        return AST.WhileStatement(condition, block)

    # TODO: Implement logic to capture multiple statements as part of a block
    def block(self):
        self.enter_scope()
        statements = []
        while self.current_token[0] != 'EOF' and self.current_token[0] != 'ELSE' and self.current_token[0] != "END":
            if self.current_token[0]== "EQUALS":
                statements.append(self.assign_stmt())
            if self.current_token[0] == "IF":
                statements.append(self.if_stmt())
            if self.current_token[0]== "WHILE":
                statements.append(self.while_stmt())
           # if self.current_token[0]== "EXPRESSION":
              #  statements.append(self.expr_stmt)
            if self.current_token[0]== "INDENTIFIER":
                statements.append(self.function_call)
            if self.current_token[0]== ["FLOAT" "INT"]:
                statements.append(self.decl_stmt())
            if self.current_token[0]== "RBRACE":
                break
            statements.append(self.statement()) #i added this, CAN DELETE

          #  elif self.current_token[0] == "WHILE":
                #statements.append(self.while_stmt())
           # else:
               # statements.append(self.statement())
      #  self.exit_scope()
            #last thing to do self.exit_scope, DOUBLE CHECK

         #enter a new scope,
        """
        Parses a block of statements. A block is a collection of statements grouped by `{}`.
        Example:

        x = 5
        y = 10

        TODO: Implement logic to capture multiple statements as part of a block.
        """
        return AST.Block(statements)

    # TODO: Implement logic to parse binary operations (e.g., addition, subtraction) with correct precedence and type checking
    def expression(self):
        """
        Parses an expression. Handles operators like +, -, etc.
        Example:
        x + y - 5
        TODO: Implement logic to parse binary operations (e.g., addition, subtraction) with correct precedence and type checking.
        """
        left = self.term()
       # if left.value_type == "IDENTIFIER":
         #   self.checkVarUse(left.value_type)

        while self.current_token[0] in ['PLUS', 'MINUS']:
            op = self.current_token[0]
            self.advance()
            right = self.term()
            if right.value_type == "IDENTIFIER":
                self.checkVarUse(right.value_type)

            self.checkTypeMatch2(left.value_type, right.value_type, left, right)
            left = AST.BinaryOperation(left, op, right, value_type=left.value_type)

        return left

    # TODO: Implement parsing for boolean expressions and check for type compatibility
    def boolean_expression(self):
        left = self.expression()
        if self.current_token[0] in ['EQ', 'GREATER', 'NEQ', 'LESS']:

        #was term
      #  while self.current_token[0] == 'EQ' or self.current_token[0] == "GREATER" or self.current_token[0] == 'NEQ' or \
            #    self.current_token[0] == 'LESS' or self.current_token[0] == 'END':  # term and bloc
            op = self.current_token  # in
            self.advance()
            right = self.expression()#was term
            self.checkTypeMatch2(left.value_type,right.value_type,left,right )
            return AST.BooleanExpression(left, op, right)

        """
        Parses a boolean expression. These are comparisons like ==, !=, <, >.
        Example:
        x == 5
        TODO: Implement parsing for boolean expressions and check for type compatibility.
        """

    # TODO: Implement parsing for multiplication and division and check for type compatibility
    def term(self):
        left = self.factor()
        while self.current_token[0] in ['MULTIPLY', 'DIVIDE']:
            op = self.current_token
            self.advance()
            right = self.factor()
            left = AST.BinaryOperation(left, op, right,value_type=left.value_type)
            self.checkTypeMatch2(left.value_type, right.value_type,left, right)
        return left
          #  left = AST.BinaryOperation(left,op, right,value_type=left.value_type )


        """
        Parses a term. A term consists of factors combined by * or /.
        Example:
        x * y / z
        TODO: Implement parsing for multiplication and division and check for type compatibility.
        """

    def factor(self):
        if self.current_token[0] == 'NUMBER':
            num= self.current_token#[1]
            self.advance()
            # handle int
            return AST.Factor(num, 'int')
        elif self.current_token[0] == 'FNUMBER':
            # handle float
            var_name = self.current_token[1]
            self.advance()
            return AST.Factor(var_name, 'float')
        elif self.current_token[0] == 'IDENTIFIER':
            var_name = self.current_token[1]
            var_type = self.get_variable_type(var_name)
            self.checkVarUse(var_name)
            self.advance()
            # TODO: Ensure that you parse the identifier correctly, retrieve its type from the symbol table, and check if it has been declared in the current or any enclosing scopes.
            return AST.Factor(var_name, var_type)  #pretty important, get name, type, check variable exist, advance once , var_name,var_type)
        elif self.current_token[0] == 'LPAREN':
            self.advance()
            expr = self.expression()
            self.expect('RPAREN')
            return expr
        else:
            raise ValueError(f"Unexpected token in factor: {self.current_token}")

    def function_call(self):
        func_name = self.current_token[1]
        self.advance()
        self.expect('LPAREN')
        args = self.arg_list()
        self.expect('RPAREN')

        return AST.FunctionCall(func_name, args)

    def arg_list(self):
        """
        Parses a list of function arguments.
        Example:
        (x, y + 5)
        """
        args = []
        if self.current_token[0] != 'RPAREN':
            args.append(self.expression())
            while self.current_token[0] == 'COMMA':
                self.advance()
                args.append(self.expression())

        return args

    def expect(self, token_type):
        if self.current_token[0] == token_type:
            self.advance()
        else:
            raise ValueError(f"Expected token {token_type}, but got {self.current_token[0]}")

    def peek(self):
        return self.tokens[0][0] if self.tokens else None
