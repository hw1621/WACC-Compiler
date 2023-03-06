package Frontend

import parsley.Parsley
import parsley.Parsley._
import parsley.implicits.character.{charLift, stringLift}
import parsley.token.descriptions.{LexicalDesc, SymbolDesc, SpaceDesc, NameDesc}
import parsley.token.descriptions.text.{TextDesc, EscapeDesc}
import parsley.character.{digit}
import parsley.token.{Lexer, predicate}
import parsley.token.predicate.{Unicode}
import parsley.token.descriptions.text.NumericEscape
import parsley.combinator.{optional}
import parsley.character.{char}
import parsley.errors.combinator._





object Lexer {
    //defination of the keyword and operator set
    val symbolDescrip = SymbolDesc.plain.copy(
        hardKeywords = Set ("begin", "end", "is", "end", "skip", 
        "read", "free", "return", "exit", "print", "println", "if", "then", 
        "else", "fi", "while", "do", "done", "begin", "end", "fst",
        "snd", "newpair", "call", "int", "bool", "char", "string",
        "pair", "true", "false", "null"
        ),
        hardOperators = Set ("!", "-", "len", "ord", "chr","*", "/", "%", "+",
        ">", ">=", "<", "<=", "==", "!=", "&&", "||")
    )
    //defination of comments
    val spaceDescrip = SpaceDesc.plain.copy(commentLine = "#")
    //defnation of identifier
    val nameDescrip = NameDesc.plain.copy(
        identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
        identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_')
    )
    //defination of escape charaters
    val escapeDescrip = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set('\'', '\"','\\'),
        singleMap = Map('n' -> 10, '0' -> 0, 'b' -> 8, 't' -> 9, 'f' -> 12, 'r' ->13),
        multiMap = Map.empty,
        decimalEscape = NumericEscape.Illegal,
        hexadecimalEscape = NumericEscape.Illegal,
        octalEscape = NumericEscape.Illegal,
        binaryEscape = NumericEscape.Illegal,
        emptyEscape = None,
        gapsSupported = false
    )
    //defination of char and strings
    val textDescrip = TextDesc.plain.copy(
        escapeSequences = escapeDescrip,
        characterLiteralEnd = '\'',
        stringEnds = Set("\""),
        multiStringEnds = Set.empty,
        graphicCharacter = Unicode(x => x >= ' '.toInt & x != '\"'.toInt & x != '\''.toInt & x != '\\'.toInt)
    )
    //build the lexer
    val lexicalDesc = LexicalDesc.plain.copy(
        spaceDesc = spaceDescrip,
        nameDesc = nameDescrip,
        symbolDesc = symbolDescrip,
        textDesc = textDescrip
    )

    var lexer = new Lexer(lexicalDesc)

    private def token[A](p: Parsley[A]) = lexer.lexeme(attempt(p))
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    val identifier = lexer.lexeme.names.identifier
    val operators = lexer.lexeme.names.userDefinedOperator.label("operator")
    val number = lexer.lexeme.numeric.natural.decimal.label("number")

    val NEGATION: Parsley[Unit] = token('-' *> notFollowedBy(digit)).label("negation")
    //use the defined string lit and char lit in the lexer built
    val stringLit = lexer.lexeme.text.string.ascii.label("string liter")
    val charLit = lexer.lexeme.text.character.ascii.label("character liter")
    val boolLit = token("true" #> true <|> "false" #> false).label("boolean liter")
    private val sign: Parsley[BigInt => BigInt] = ((char('-') #> { x: BigInt => -x }) <|> (optional('+') #> {x: BigInt => x})).label("int sign")
    private def checkOverflow (x: BigInt): Boolean = !x.isValidInt
    val INT = token(sign <*> number).filterNot(checkOverflow).map(_.intValue).label("int liter").explain("valid int literal format is: int sign + number")

    val implicits = lexer.lexeme.symbol.implicits
}