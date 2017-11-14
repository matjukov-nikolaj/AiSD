PROGRAM InfixInPostfixAndCalculate(INPUT, OUTPUT);
{   25. В строке текстового файла задано выражение
из целых чисел и операций '+', '-', '*', '/', '^'.
Порядок    вычислений   определяется   приоритетом
операций и круглыми скобками. Возможен одноместный
минус  в  начале  выражения  или после открывающей
скобки.   Преобразовать  выражение  в  постфиксную
форму   (алгоритм   Дейкстры)   и   вычислить  его
значение. Показать этапы выполнения (11).

Матюков Николай Юрьевич ПС-21
}

USES
  Stack;
  
CONST
  ENTER_THE_EXPRESSION = 'Введите выражение: ';
  SPACE = ' ';
  DEGREE = '^';
  ADDITION = '+';
  DIFFERENCE = '-';
  MULTIP = '*';
  DIVISION = '/';
  OPEN_BRACKET = '(';
  CLOSE_BRACKET = ')';
  SIGN = [ADDITION, DIFFERENCE, MULTIP, DIVISION, DEGREE];
  DIGIT = ['0' .. '9']; 
  ZERO_STR = '0';
  EMPTY_STR = '';
    

VAR 
  ResultString, CurrentString: STRING;
  SizeCurrentString, I: INTEGER;
  CurrentCh: CHAR;

PROCEDURE RemovalFirstSpace(VAR Ch: CHAR);
BEGIN
  IF NOT EOLN(INPUT)
  THEN
    READ(INPUT, Ch);
  IF Ch = SPACE
  THEN
    WHILE Ch = SPACE
    DO
      BEGIN
        READ(INPUT, Ch); 
      END;
END;

FUNCTION GetInputString: STRING;
VAR
  Ch: CHAR;
  CurrentString: STRING;
BEGIN
  Ch := SPACE;
  CurrentString := EMPTY_STR;
  RemovalFirstSpace(Ch);
  WHILE (NOT EOLN(INPUT))
  DO
    BEGIN
      IF Ch <> SPACE
      THEN
        CurrentString := CurrentString + Ch;
      READ(INPUT, Ch);
    END;
  IF EOLN(INPUT)
  THEN
    BEGIN
      IF (Ch <> SPACE)
      THEN
        CurrentString := CurrentString + Ch;
    END;
  GetInputString := CurrentString;
END;

PROCEDURE CheckTopCharOnOpenBracket;
VAR
  Ch: CHAR;
BEGIN
  IF (TopChar = OPEN_BRACKET)
  THEN
    Ch := PopChar;
  Ch := SPACE; 
END;

FUNCTION FromStackInout(VAR ResultString: STRING): STRING;
VAR
  Ch: CHAR;
BEGIN
  WHILE NOT(IsEmpty)
  DO
    BEGIN 
      CheckTopCharOnOpenBracket;
      Ch := PopChar;
      ResultString := ResultString + SPACE + Ch;   
    END;
  FromStackInout := ResultString; 
END;

FUNCTION GetBeforeEndOrOpenBracket(VAR ResultString: STRING; FromClose: BOOLEAN): STRING;
VAR
  Ch: CHAR;
BEGIN
  WHILE (TopChar <> OPEN_BRACKET) AND (NOT IsEmpty)
  DO
    BEGIN 
      Ch := PopChar;  
      ResultString := ResultString + SPACE + Ch;
    END;
  IF FromClose
  THEN
    CheckTopCharOnOpenBracket;
  GetBeforeEndOrOpenBracket := ResultString;
END;

PROCEDURE PushAddOrDiff; 
BEGIN  
  IF (TopChar <> MULTIP) AND (TopChar <> DIVISION)
    AND (TopChar <> DEGREE) AND (TopChar <> ADDITION)
    AND (TopChar <> DIFFERENCE)
  THEN
    PushChar(CurrentCh)
  ELSE
    BEGIN
      ResultString := GetBeforeEndOrOpenBracket(ResultString, FALSE);
      PushChar(CurrentCh);
    END; 
END;

PROCEDURE PushMultOrDiv;
VAR
  Ch: CHAR;  
BEGIN 
  IF (TopChar <> DEGREE) AND (TopChar <> DIVISION) AND (TopChar <> MULTIP)
  THEN 
    PushChar(CurrentCh)
  ELSE
    BEGIN
      WHILE (TopChar <> OPEN_BRACKET) AND (NOT IsEmpty)
            AND (TopChar <> ADDITION) AND (TopChar <> DIFFERENCE)
      DO
        BEGIN 
          Ch := PopChar;  
          ResultString := ResultString + SPACE + Ch;
        END; 
      CheckTopCharOnOpenBracket; 
      PushChar(CurrentCh);
    END;
END;

PROCEDURE CheckUnaryMinus(VAR ResultString, CurrentString: STRING);
BEGIN
  IF (TopChar = OPEN_BRACKET) AND NOT (CurrentString[I - 1] IN DIGIT) OR (ResultString = EMPTY_STR)
  THEN
    ResultString := ResultString + ZERO_STR;
END; 

PROCEDURE CalculateExpression(ResStr: STRING); 
VAR
  SizePostfixStr, I, J, FirstNum, SecondNum, CurrResult, Code, CurrNum: INTEGER;
  Ch: CHAR;
  CurrNumberStr, PostfixStr: STRING;
BEGIN
  Code := 0;
  CurrResult := 0;
  CurrNum := 0;
  CurrNumberStr := EMPTY_STR;
  PostfixStr := ResStr; 
  SizePostfixStr := length(PostfixStr); 
  FOR I := 1 TO SizePostfixStr
  DO
    BEGIN
      Ch := PostfixStr[I];
      IF Ch IN DIGIT
      THEN     
        CurrNumberStr := CurrNumberStr + Ch;
      IF Ch = SPACE
      THEN
        BEGIN  
          IF CurrNumberStr <> EMPTY_STR
          THEN
            BEGIN
              Val(CurrNumberStr, CurrNum, Code);
              PushInt(CurrNum);
            END;
          CurrNum := 0;
          CurrNumberStr := EMPTY_STR;
        END;
      IF Ch IN SIGN
      THEN
        BEGIN 
          SecondNum := PopInt;
          FirstNum := PopInt;
          IF Ch = ADDITION
          THEN
            CurrResult := FirstNum + SecondNum;
          IF Ch = DIFFERENCE
          THEN
            CurrResult := FirstNum - SecondNum;
          IF Ch = MULTIP
          THEN
            CurrResult := FirstNum * SecondNum;
          IF Ch = DIVISION
          THEN
            CurrResult := FirstNum DIV SecondNum;
          IF Ch = DEGREE
          THEN
            BEGIN
              CurrResult := 1;
              J := 0;
              FOR J := 1 TO SecondNum
              DO
                CurrResult := CurrResult * FirstNum
            END;
          PushInt(CurrResult);
          CurrResult := 0; 
        END; 
    END;
  WRITELN('Answer = ', TopInt);
END;

PROCEDURE Main;
BEGIN
  CurrentString := EMPTY_STR;
  ResultString := EMPTY_STR;
  CurrentCh := SPACE;
  SizeCurrentString := 0;
  WRITELN(ENTER_THE_EXPRESSION);
  CurrentString := GetInputString;
  SizeCurrentString := length(CurrentString); 
  FOR I := 1 TO SizeCurrentString
  DO 
    BEGIN
      CurrentCh := CurrentString[I];
      CASE CurrentCh OF
        OPEN_BRACKET: PushChar(CurrentCh);
        CLOSE_BRACKET: ResultString := GetBeforeEndOrOpenBracket(ResultString, TRUE);
        ADDITION: PushAddOrDiff;                   
        DIFFERENCE: 
          BEGIN
            CheckUnaryMinus(ResultString, CurrentString);
            PushAddOrDiff;
          END;  
        MULTIP: PushMultOrDiv;
        DIVISION: PushMultOrDiv;
        DEGREE:  PushChar(CurrentCh);
        ELSE
          ResultString := ResultString + CurrentCh; 
      END;
      WRITE('Input: ', CurrentCh);
      WRITE(' Result: ', ResultString);
      WRITE(' Stack: ');
      StackCharsInOut;
      IF (CurrentCh IN SIGN)
      THEN
        ResultString := ResultString + SPACE;         
    END;
  ResultString := FromStackInout(ResultString);
  WRITELN('PostfixForm: ', ResultString);  
  CalculateExpression(ResultString);
END; 

BEGIN
  Main;
END.


