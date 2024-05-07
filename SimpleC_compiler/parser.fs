//
// Parser for SimpleC programs. This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC. The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
//
// Modified by:
//   Tyler Strach
//   UIC Spring 2024 CS 341
//

namespace compiler

module parser =
  // use with the identifier(“identifier"), string literal(“str_literal”), int literal(“int_literal”)
  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern) 

  //
  // matchToken
  //
  let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens
    // first checks if it is an identifier or literal due to the special prefix
    if expected_token = "identifier" || expected_token = "str_literal" || expected_token = "int_literal" || expected_token = "real_literal" then 
      if beginswith expected_token next_token then
        // printfn "%A" next_token
        List.tail tokens // if it is then remove head and return rest of tokens
      else
        failwith ("expecting " + expected_token + ", but found " + next_token)
    elif expected_token = next_token then // normal case of matching  
      // printfn "%A" next_token
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  // legal to have an empty line of just ";"
  let private empty tokens = 
    matchToken ";" tokens

  // legal order when declaring a variable (no value attached)
  let private vardecl tokens = 
    let curToken = List.head tokens
    if curToken = "int" then
      matchToken "int" tokens |> matchToken "identifier" |> matchToken ";"
    else 
      matchToken "real" tokens |> matchToken "identifier" |> matchToken ";"

  // legal order when inputting into an identifier (variable)
  let private input tokens = 
    matchToken "cin" tokens |> matchToken ">>" |> matchToken "identifier" |> matchToken ";"

  // legal options on values to output
  let private expr_value tokens = 
    let curToken = List.head tokens
    if beginswith "identifier" curToken then
      matchToken "identifier" tokens
    elif beginswith "int_literal" curToken then
      matchToken "int_literal" tokens
    elif beginswith "str_literal" curToken then
      matchToken "str_literal" tokens
    elif beginswith "real_literal" curToken then
      matchToken "real_literal" tokens
    elif curToken = "true" then
      matchToken "true" tokens
    elif curToken = "false" then
      matchToken "false" tokens
    else // if none of the legal options, fail
      failwith ("expecting identifier or literal, but found " + curToken)
  
  // legal options when using cout
  let private output_value tokens = 
    let curToken = List.head tokens
    if curToken = "endl" then
      matchToken "endl" tokens
    else
      expr_value tokens // calls the rest of the options if not endl

  // legal order when outputting 
  let private output tokens = 
    matchToken "cout" tokens |> matchToken "<<" |> output_value |> matchToken ";"

  // legal operations when assigning an identifier (variable)
  let private expr_op tokens = 
    let curToken = List.head tokens
    if curToken = "+" then
      matchToken "+" tokens
    elif curToken = "-" then
      matchToken "-" tokens
    elif curToken = "*" then 
      matchToken "*" tokens
    elif curToken = "/" then
      matchToken "/" tokens
    elif curToken = "^" then
      matchToken "^" tokens
    elif curToken = "<" then
      matchToken "<" tokens
    elif curToken = "<=" then
      matchToken "<=" tokens
    elif curToken = ">" then
      matchToken ">" tokens
    elif curToken = ">=" then
      matchToken ">=" tokens
    elif curToken = "==" then
      matchToken "==" tokens
    elif curToken = "!=" then
      matchToken "!=" tokens
    else
      failwith ("expecting expression operator, but found " + curToken)

  // legal options when assigning an identifier (varibale)
  let private expr tokens = 
    let newTokens = expr_value tokens
    let newHead = List.head newTokens
    if newHead = "+" || newHead = "-" || newHead = "*" || newHead = "/" || newHead = "^" || 
       newHead = "<" || newHead = "<=" || newHead = ">" || newHead = ">=" || newHead = "==" || newHead = "!=" then
       expr_op newTokens |> expr_value
    else // can just be 1 value without operations
      newTokens

  // legal order when assigning an identifier (variable) to an expression
  let private assignment tokens = 
    // printfn "%A" List.head
    matchToken "identifier" tokens |> matchToken "=" |> expr |> matchToken ";"

  // legal order when assigning an identifier (variable) to an expression
  let private condition tokens = expr tokens

  // main driver function which checks the first token of each new line of code 
  // and matches to the possible statement function
  let rec private stmt tokens = 
    let curToken = List.head tokens
    // printfn "%A" curToken
    if curToken = ";" then 
      empty tokens
    elif curToken = "int" || curToken = "real" then
      vardecl tokens
    elif curToken = "cin" then 
      input tokens
    elif curToken = "cout" then
      output tokens
    elif beginswith "identifier" curToken then
      assignment tokens
    elif curToken = "if" then
      ifstmt tokens
    else 
      failwith ("expecting statement, but found " + curToken)

  // legal order of then_part, which is just a legal statement
  and private then_part tokens = stmt tokens

  // legal order of the else_part, which can be either else <stmt> or empty
  and private else_part tokens =  
    let newHead = List.head tokens
    if newHead = "else" then
      matchToken "else" tokens |> stmt
    else 
      tokens

  // legal order of any ifstmt 
  and private ifstmt tokens = 
    matchToken "if" tokens |> matchToken "(" |> condition |> matchToken ")" |> then_part |> else_part

  // main recursive function that calls stmt over and over again if the code is
  // not over yet, EMPTY = "}" as the next token which is base case
  let rec private morestmts tokens = 
    let curToken = List.head tokens
    if curToken = "}" then
      tokens
    else
      stmt tokens |> morestmts
    
  // first call that goes through the body of the code
  let rec private stmts tokens =
    stmt tokens |> morestmts 

  // main order of code, stmts is body of main
  let private simpleC tokens = 
    matchToken "void" tokens
    |> matchToken "main"
    |> matchToken "("
    |> matchToken ")"
    |> matchToken "{"
    |> stmts
    |> matchToken "}"
    |> matchToken "$"   

  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "Success!"
    with 
      | ex -> "syntax_error: " + ex.Message