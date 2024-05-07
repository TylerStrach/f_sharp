//
// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid SimpleC program.
//
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 202
//
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024
//
// Modified by:
//   Tyler Strach
//   UIC Spring 2024 CS 341

namespace compiler

module checker =
  // use with the identifier(“identifier"), string literal(“str_literal”), 
  // int literal(“int_literal”), real literal("real_literal") 
  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern) 

  // removes the "indentifier:" portion from the token variable 
  let removePrefix (str: string) (prefix: string) =
    if str.StartsWith(prefix) then
      str.[prefix.Length..] // Return the substring starting from the length of the prefix
    else
      str 

  // function that retrieves the type from symbol table given a variable name vName
  let rec private getType symboltable vName : string = 
    match symboltable with
    | [] -> "" // Return an empty string if the symbol table is empty which should not happen
    | (curV, curT)::rest when curV = vName -> curT
    | _::rest -> getType rest vName 

  // returns the type of the literal or variable given
  let private expr_value tokens symboltable : string = 
    let curToken = List.head tokens
    if beginswith "identifier" curToken then
      getType symboltable (removePrefix curToken "identifier:")
    elif beginswith "int_literal" curToken then
      "int"
    elif beginswith "str_literal" curToken then
      "str"
    elif beginswith "real_literal" curToken then
      "real"
    elif curToken = "true" then
      "bool"
    elif curToken = "false" then
      "bool"
    else 
      "something went wrong with expr_value"


  // type checking all expressions
  // checks if the variable type and expression type are the same
  // checks if the expressions on both sides of an operator are the correct type and/or equal
  let private expr tokens symboltable = 
    let firstType = expr_value tokens symboltable
    let newTokens = List.tail tokens
    let newHead = List.head newTokens

    // checking rule 2 for arithmetic operations
    if newHead = "+" || newHead = "-" || newHead = "*" || newHead = "/" || newHead = "^" then
      let newNewTokens = List.tail newTokens
      let secondType =  expr_value newNewTokens symboltable // retrieve second type
      // both are either ints or reals
      if (firstType = "int" && secondType = "int") || (firstType = "real" && secondType = "real") then
        let finalTokens = List.tail newNewTokens
        (finalTokens, firstType)
      else 
        failwith("operator " + newHead + " must involve 'int' or 'real'") // fails rule 2 if not

    // checking rule 3 for comparison operations
    elif newHead = "<" || newHead = "<=" || newHead = ">" || newHead = ">=" || newHead = "==" || newHead = "!=" then
      let newNewTokens = List.tail newTokens
      let secondType =  expr_value newNewTokens symboltable // retrieve second type
      // both are the same type
      if firstType = secondType then
        if firstType = "real" && newHead = "==" then
          printfn "warning: comparing real numbers with == may never be true" // covering rule 6 and still continuing
        let finalTokens = List.tail newNewTokens
        (finalTokens, "bool") // returning bool because correct format and operator
      else
        failwith("type mismatch '" + firstType + "' " + newHead + " '" + secondType + "'") // fails rule 3 if not same type

    else // can just be 1 value without operations
      (newTokens, firstType)

  // verifying the assignment type with the type of the variable, returns next tokens after verification
  let private assignment tokens symboltable = 
    let varName = List.head tokens
    let varType = getType symboltable (removePrefix varName "identifier:") // get the type of the variable currently being assigned 
    let newTokens = List.tail (List.tail tokens)

    let (finalTokens, exprType) = expr newTokens symboltable // call expr and confirm the expression is of matching type
    if varType <> exprType then
      if exprType = "int" && varType = "real" then 
        finalTokens // special case of assigining int to real
      else
        failwith("cannot assign '" + exprType + "' to variable of type '" + varType + "'") // rule 4 fails
    else
      finalTokens

  // check for correct type of if statments
  let private condition tokens symboltable = 
    let (finalTokens, exprType) = expr tokens symboltable // call expr and confirm it is a bool type
    if exprType <> "bool" then
      failwith("if condition must be 'bool', but found '" + exprType + "'") // rule 5
    else
      finalTokens

  // main function that iterates through all tokens, checks for assignments and if statements where there are expressions
  let rec private stmt tokens symboltable = 
    let curToken = List.head tokens
    // printfn "curToken: %A" curToken
    if curToken = "int" || curToken = "real" then
      List.tail (List.tail tokens) // if token is a variable declaration, skip because already checked this in analyzer
    elif beginswith "identifier" curToken && List.head (List.tail tokens) = "=" then
      assignment tokens symboltable  
    elif curToken = "if" then
      let newTokens = List.tail (List.tail tokens)
      condition newTokens symboltable
    else // if nothing to do with types, then just pop off the first token and continue
      List.tail tokens

  // main recursive function that calls stmt over and over again if the code is
  // not over yet, EMPTY = "$" as the next token which is base case
  let rec private morestmts tokens symboltable = 
    let curToken = List.head tokens
    if curToken = "$" then
      tokens
    else
      let newTokens = stmt tokens symboltable
      morestmts newTokens symboltable
    
  // first call that goes through the body of the code
  let rec private stmts tokens symboltable =
    let newTokens = stmt tokens symboltable
    morestmts newTokens symboltable

  // calls stmts which will recusively loop through all tokens until EOF
  let private simpleC tokens symboltable = 
    stmts tokens symboltable

  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

