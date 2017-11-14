PROGRAM TextAlignmentWidth(INPUT, OUTPUT);
{   22.  В  текстовом  файле  записан  отдельный абзац.
Переносовслов  нет.  Выровнять строки абзаца по ширине.
Иными  словами,правые  границы  строк  выравниваются по
заданной позиции за счетвставки дополнительных пробелов
между   словами.   Первая   строкаабзаца  должна  иметь
заданный  отступ, а остальные строки должныначинаться с
первой  позиции. Последняя строка абзаца поправому краю
не  выравнивается.  Число  строк  в  исходном иконечном
файлах может отличаться (8).

Выполнил: Матюков Николай
ПС-21

}
CONST
  NO_FILE = 'Не указан исходный файл';
  ERROR_OPEN_FILE = 'Ошибка открытия файла ';
  MAX_LENGTH_STRING_ALERT = 'Введите максимальную длинну строки: ';
  MIN_NUMBER_SYMBOLS_ALERT = 'Error: Слишком маленькая длина строки.';
  BIG_WORD_ALERT = 'Имеется слово, которое больше максимальной длинны строки';
  SPACE = ' ';
  FIRST_INDENT = 4;
  FIRST_INDENT_SPACE = '    ';
  MAX_ARRAY_SIZE = 100;
  MIN_ARRAY_SIZE = 1;
  RIGHT_GAP = 1;
  MIN_NUMBER_SYMBOLS = 10;
TYPE 
  BufferArraySize = MIN_ARRAY_SIZE .. MAX_ARRAY_SIZE;  
  BufferWord = ARRAY[BufferArraySize] OF STRING;
VAR
  FIn, FOut: TEXT;
 
PROCEDURE WriteStringInOut(Alert: STRING);
BEGIN {WriteStringInOut}
  WRITELN(Alert);
  READLN;
  Halt;
END; {WriteStringInOut}

PROCEDURE ErrorOpenFile(VAR FileName: STRING);
BEGIN
  IF IoResult <> 0 
  THEN
    WriteStringInOut(ERROR_OPEN_FILE + FileName);
END;

PROCEDURE CheckEnteredFile(VAR FIn, FOut:TEXT);
VAR 
  FileName: STRING;
BEGIN {CheckEnteredFile}
  IF ParamCount < 2
  THEN
    WriteStringInOut(NO_FILE)
  ELSE
    BEGIN
      ASSIGN(FOut, ParamStr(2));
      ASSIGN(FIn, ParamStr(1));
    END;
  FileName := ParamStr(1);
  {$I-}
  RESET(FIn);           
  {$I+}  
  ErrorOpenFile(FileName);
  FileName := ParamStr(2);
  {$I-}
  REWRITE(FOut);
  {$I+}
  ErrorOpenFile(FileName);
END; {CheckEnteredFile}

PROCEDURE RemovalFirstSpace(VAR FIn: TEXT; VAR Ch: CHAR);
BEGIN
  IF NOT EOLN(FIn)
  THEN
    READ(FIn, Ch);
  IF Ch = SPACE
  THEN
    WHILE Ch = SPACE
    DO
      BEGIN
        READ(FIn, Ch); 
      END;
END;

FUNCTION WordsReader(VAR FIn:TEXT): STRING;
VAR
  Ch: CHAR;
  CurrentWord: STRING;
BEGIN
  Ch := SPACE;
  CurrentWord := '';
  RemovalFirstSpace(FIn, Ch);
  WHILE (NOT EOLN(FIn)) AND (Ch <> SPACE)
  DO
    BEGIN
      CurrentWord := CurrentWord + Ch;
      READ(FIn, Ch);
    END;
  IF EOLN(FIn)
  THEN
    BEGIN
      IF (Ch <> SPACE)
      THEN
        CurrentWord := CurrentWord + Ch;
      READLN(FIn);
    END;
  WordsReader := CurrentWord;
END;

PROCEDURE WriteBuffer(VAR BufferArray: BufferWord; CurrArrayEl: INTEGER);
VAR
  I: INTEGER;
BEGIN 
  I := 1;
  WHILE I <> CurrArrayEl
  DO
    BEGIN
      WRITE(FOut, BufferArray[I]);
      WRITE(BufferArray[I]);
      INC(I);
    END;
END;

PROCEDURE InsertAdditionalGaps(VAR BufferArray: BufferWord; CurrArrayEl: INTEGER; NumGapsForInsert: INTEGER);
VAR 
  I: INTEGER;
BEGIN
  I := 2;
  WHILE NumGapsForInsert > 0
  DO
    BEGIN
      IF (CurrArrayEl = 2) AND (BufferArray[CurrArrayEl] = '')
      THEN
        DEC(CurrArrayEl);
      IF CurrArrayEl <> 1
      THEN
        BEGIN
          IF I < CurrArrayEl
          THEN
            BEGIN
              BufferArray[I] := ' ' + BufferArray[I];
              DEC(NumGapsForInsert);
              INC(I);
            END            
          ELSE
            I := 2
        END
      ELSE
        BEGIN
          NumGapsForInsert := NumGapsForInsert DIV 2;
          WHILE NumGapsForInsert > 0
          DO
            BEGIN
              I := 1;
              BufferArray[CurrArrayEl] := SPACE + BufferArray[CurrArrayEl];
              DEC(NumGapsForInsert);
            END;      
        END;            
    END;
END;

PROCEDURE CleanBuffer(VAR BufferArray: BufferWord);
VAR
  I: INTEGER;
BEGIN
  I := 1;
  WHILE I <> MAX_ARRAY_SIZE
  DO
    BEGIN
      BufferArray[I] := '';
      INC(I);
    END;
END;

FUNCTION GetMaxNum: INTEGER;
VAR
  MaxNumSymbols: INTEGER;
BEGIN
  WRITELN(MAX_LENGTH_STRING_ALERT);
  READLN(MaxNumSymbols);
  GetMaxNum := MaxNumSymbols;
END;

PROCEDURE TextAlignment(VAR FIn, FOut: TEXT);
VAR
  MaxNumSymbols, CurrNumSymbols, CurrArrayEl, NumGapsForInsert: INTEGER;
  Buffer: BufferWord;
  CurrentWord: STRING;
  IsFirstWord: BOOLEAN;
  Ch: CHAR;
BEGIN
  CurrNumSymbols := 0;
  CurrArrayEl := 0;
  NumGapsForInsert := 0;
  IsFirstWord := TRUE;
  Ch := SPACE;
  CurrNumSymbols := FIRST_INDENT;
  CurrentWord := '';
  MaxNumSymbols := GetMaxNum;
  WHILE NOT EOF(FIn)
  DO
    BEGIN
      CurrentWord := WordsReader(FIn);
      INC(CurrArrayEl);
      CurrNumSymbols := CurrNumSymbols + length(CurrentWord); 
      IF (MaxNumSymbols <= MIN_NUMBER_SYMBOLS)
      THEN
        BEGIN
          WRITELN(MIN_NUMBER_SYMBOLS_ALERT);
          BREAK;
        END;
      IF (length(CurrentWord) >= MaxNumSymbols)
      THEN
        BEGIN
          WRITELN(BIG_WORD_ALERT);
          BREAK;
        END;
      IF (CurrNumSymbols <= MaxNumSymbols)
      THEN
        BEGIN
          IF (IsFirstWord)
          THEN
            BEGIN
              Buffer[CurrArrayEl] := FIRST_INDENT_SPACE + CurrentWord;
              IsFirstWord := FALSE;
            END
          ELSE
            BEGIN
              Buffer[CurrArrayEl] := ' ' + CurrentWord;
            END;
          CurrNumSymbols := CurrNumSymbols + RIGHT_GAP;
        END
      ELSE
        BEGIN
          NumGapsForInsert := MaxNumSymbols - (CurrNumSymbols - length(CurrentWord) - RIGHT_GAP);
          InsertAdditionalGaps(Buffer, CurrArrayEl, NumGapsForInsert);          
          WriteBuffer(Buffer, CurrArrayEl);
          WRITELN(FOut);
          WRITELN;
          CurrNumSymbols := length(CurrentWord) + RIGHT_GAP;
          CleanBuffer(Buffer);
          CurrArrayEl := 1;
          Buffer[CurrArrayEl] := CurrentWord; 
        END;   
    END;
  IF (MaxNumSymbols > MIN_NUMBER_SYMBOLS)
  THEN
    BEGIN
      WriteBuffer(Buffer, CurrArrayEl);
      WRITELN(FOut, SPACE, CurrentWord);
      WRITELN(SPACE, CurrentWord);
    END
END;  
  

BEGIN {TextAlignWidth} 
  CheckEnteredFile(FIn, FOut); 
  TextAlignment(FIn, FOut);
END. {TextAlignWidth}
