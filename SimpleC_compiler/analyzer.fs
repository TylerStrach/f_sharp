//
// Analyzer for SimpleC programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
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

module analyzer =
  // given new variable to be declared, checks if it already exists in symbol table
  let rec checkDuplicate table newVar = 
    let (varName, varType) = newVar
    match table with
    | [] -> false // if the variable isn't found, then NOT a duplicate
    | (v, t)::rest -> if varName = v then 
                        true // if found, then there IS a duplicate
                      else
                        checkDuplicate rest newVar

  // removes the "indentifier:" portion from the token variable 
  let removePrefix (str: string) (prefix: string) =
    if str.StartsWith(prefix) then
      str.[prefix.Length..] // Return the substring starting from the length of the prefix
    else
      str // Return the original string if it doesn't start with the prefix

  // use with the identifier(“identifier")
  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern) 

  // prepends the new variable to the table and removes the line of code from the list of tokens
  let addVar tokens table = 
    match tokens with
    | [] -> failwith "Not enough tokens to add a variable"
    | [varType] -> failwith "Not enough tokens to add a variable"
    | [varType; varName] -> failwith "Not enough tokens to add a variable"
    | t::v::n::rest -> 
        let varName = removePrefix v "identifier:"
        if checkDuplicate table (varName, t) then
            failwith ("redefinition of variable '" + varName + "'")
        else
            (rest, (varName, t)::table)

  // main driver function which next token, if a declaration, will parse the line and add to the able
  // if any variable is mentioned, verifies that it has been declared.
  // if token does not relate to declaration or variable, just moves on to the next token
  let rec private stmt tokens table = 
    let curToken = List.head tokens
    if curToken = "int" || curToken = "real" then
      addVar tokens table
    elif beginswith "identifier:" curToken then
      let varName = removePrefix curToken "identifier:"
      if(checkDuplicate table (varName, "nullType")) then 
        (List.tail tokens, table)
      else
        failwith ("variable '" + varName + "' undefined")
    else // if nothing to do with variables, then just pop off the first token and continue
      (List.tail tokens, table)

  // main recursive function that calls stmt over and over again if the code is
  // not over yet, EMPTY = "$" as the next token which is base case
  let rec private morestmts tokens table = 
    let curToken = List.head tokens
    if curToken = "$" then
      (tokens, table)
    else
      let (newTokens, newTable) = stmt tokens table
      morestmts newTokens newTable
    
  // first call that goes through the body of the code
  let rec private stmts tokens =
    let (newTokens, newTable) = stmt tokens []
    morestmts newTokens newTable

  // calls stmts to recurse through the code looking for variable declarations and usage
  let private simpleC tokens = stmts tokens

  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
