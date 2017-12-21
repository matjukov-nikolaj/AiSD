PROGRAM AlgoritmPrima(INPUT, OUTPUT);
{   20. Реализовать  алгоритм   Прима   нахождения   остовного 
дерева и проиллюстрировать по шагам этапы его выполнения (10).

                 Матюков Николай Юрьевич ПС-21
}
CONST
  NO_FILE = 'Не указан исходный файл';
  ERROR_OPEN_FILE = 'Ошибка открытия файла ';
  SPACE = ' ';
  FirstNodeInfo = 1;
  SecondNodeInfo = 2;
  NodeCost = 3;
  

TYPE
  InfoSize = 1..3;
  NodeInfo = ARRAY [InfoSize] OF INTEGER;
  NodePtr = ^Node;
  Node = RECORD
           Next: NodePtr;
           Key: NodeInfo;
         END;
  TableInfo = RECORD
               First, Second, Cost: INTEGER;
               Tag: BOOLEAN;
             END;
VAR
  NewPtr: NodePtr;
  FIn, FOut: TEXT;
  NodeCount: INTEGER;
  NodeArr: ARRAY OF NodePtr;
  SetChosenNodes: SET OF BYTE;

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

PROCEDURE CreateNewNodeConnection(Node: NodeInfo);
BEGIN
  NEW(NewPtr);
  NewPtr^.Key := Node; 
  NewPtr^.Next := NIL;
END;

PROCEDURE WriteNodeConnection;
VAR
  I: INTEGER;
BEGIN
  I := 1;
  WHILE NodeArr[I] <> NIL
  DO
    BEGIN
      WRITELN(NodeArr[I]^.Key[FirstNodeInfo], ' ',
      NodeArr[I]^.Key[SecondNodeInfo], ' ',
      NodeArr[I]^.Key[NodeCost]);
      NodeArr[I] := NodeArr[I]^.Next;
    END;
END;

PROCEDURE ReadGraph(VAR FIn: TEXT);
VAR
  CurrNum, I: INTEGER;
  NodesInfo: NodeInfo;
  IsFirst: BOOLEAN;
  Node: NodePtr;
BEGIN
  READLN(FIn, NodeCount);
  SetLength(NodeArr, NodeCount + 1);
  IsFirst := TRUE;
  WHILE NOT EOF(FIn)
  DO
    BEGIN
      I := 1;
      WHILE NOT EOLN(FIn)
      DO
        BEGIN
          READ(FIn, CurrNum);
          NodesInfo[I] := CurrNum;
          INC(I);
        END;
      CreateNewNodeConnection(NodesInfo);
      IF IsFirst
      THEN
        BEGIN
          Node := NewPtr;
          IsFirst := FALSE;
        END;
      IF NOT IsFirst
      THEN
        BEGIN
          Node^.Next := NewPtr;
        END;
      IF NodesInfo[3] = 1
      THEN 
      BREAK;
      {ELSE
        WHILE NodeArr[CurrNum]^.Next <> NIL
        DO
          BEGIN
            NodeArr[CurrNum] := NodeArr[I]^.Next;
            WRITELN('.i.', NodeArr[CurrNum]^.Key[SecondNodeInfo]);
          END;
      IF NodeArr[CurrNum]^.Next = NIL
      THEN
        NodeArr[CurrNum]^.Next := NewPtr;}
      READLN(FIn);
    END;
  NodeArr[1] := Node;
  WriteNodeConnection;
  CLOSE(FIn);
END;

PROCEDURE PrintTable(Table: ARRAY OF TableInfo);
VAR
  I: INTEGER;
BEGIN
  FOR I := 2 TO (Length(Table) - 1)
  DO
    BEGIN
      IF Table[I].First = MAXINT
      THEN
        WRITE('MAXINT | ')
      ELSE
        BEGIN
      IF I = 2
      THEN
        WRITE(' | ');
      IF NOT Table[I].Tag 
      THEN
        WRITE(Table[I].First, ' ', 
        Table[I].Second, ' (', Table[I].Cost, ') | ')
      ELSE
        WRITE(Table[I].First, ' ', 
        Table[I].Second, ' (', Table[I].Cost, ') * | ');
        END;
    END;
  WRITELN;
END;

PROCEDURE CheckAllSelected(Table: ARRAY OF TableInfo;VAR AllSelected: BOOLEAN);
VAR
  I, TagCounter: INTEGER;
BEGIN
  TagCounter := 0;
  FOR I := 2 TO (Length(Table) - 1)
  DO
    IF Table[I].Tag
    THEN
      INC(TagCounter);
  IF TagCounter = NodeCount - 1
  THEN
    AllSelected := TRUE;
END;

PROCEDURE InitTable(VAR Table: ARRAY OF TableInfo);
VAR
  I: INTEGER;
BEGIN
  FOR I := 2 TO (Length(Table) - 1)
  DO
    BEGIN
      Table[I].First := MAXINT;
      Table[I].Second := MAXINT;
      Table[I].Cost := MAXINT;
      Table[I].Tag := FALSE;
    END;
END;

PROCEDURE InsertInTable(VAR Node: NodePtr;VAR Table: ARRAY OF TableInfo; VAR CurrNode, I: INTEGER);
BEGIN
  WHILE (Node^.Key[FirstNodeInfo] = CurrNode)
  DO
    BEGIN
      IF (Table[I].Tag = FALSE)
        AND (Node^.Key[NodeCost] < Table[I].Cost)
        AND NOT (Node^.Key[SecondNodeInfo] IN SetChosenNodes)
      THEN
        BEGIN
          Table[I].First := Node^.Key[FirstNodeInfo];
          Table[I].Second := Node^.Key[SecondNodeInfo];
          Table[I].Cost := Node^.Key[NodeCost];
        END;
      IF Node^.Next <> NIL
      THEN
        Node := Node^.Next
      ELSE
        BREAK;
      I := Node^.Key[SecondNodeInfo];
    END;
END;

PROCEDURE GetNodeWithMinCost(VAR MinCost, NodeMinCost: INTEGER; VAR Table: ARRAY OF TableInfo);
VAR
  I: INTEGER;
BEGIN
  I := 2;
  MinCost := MAXINT;
  FOR I := 2 TO Length(Table) - 1
  DO 
    BEGIN
	IF ((Table[I].Tag = FALSE) AND (MinCost > Table[I].Cost))
	THEN
	  BEGIN
	    MinCost := Table[I].Cost;
		NodeMinCost := Table[I].Second;
	  END
	END;
  FOR I := 2 TO Length(Table) - 1 
  DO
    BEGIN
      IF NodeMinCost = Table[I].Second
      THEN
        BEGIN
          Table[I].Tag := TRUE;
          WRITELN('NodesWithMinCost: ', 
          Table[I].First, ' -> ', Table[I].Second, 
          ' Cost = ', Table[I].Cost);
        END;
    END;
END;

PROCEDURE PrintMinimalOstovnyTree(VAR Table: ARRAY OF TableInfo);
VAR 
  I: INTEGER;
BEGIN
  WRITELN('The received minimum ostovny tree: ');
  FOR I := 2 TO Length(Table) - 1 
  DO
    BEGIN
       WRITELN(Table[I].First, ' ', 
        Table[I].Second, ' (', 
        Table[I].Cost, ')');
       WRITELN(FOut, Table[I].First, ' ', 
        Table[I].Second, ' (', 
        Table[I].Cost, ')');
    END;  
  CLOSE(FOut);
END;

PROCEDURE GetOstovnyTree(FirstPtr: NodePtr);
VAR
  CurrNode, I, NodeMinCost, MinCost: INTEGER;
  Node: NodePtr;
  AllSelected: BOOLEAN;
  TableOfNodes: ARRAY OF TableInfo;
BEGIN
  SetLength(TableOfNodes, NodeCount + 1);
  InitTable(TableOfNodes);
  AllSelected := FALSE;
  Node := FirstPtr;
  CurrNode := Node^.Key[FirstNodeInfo];
  I := Node^.Key[SecondNodeInfo]; 
  INCLUDE(SetChosenNodes, Node^.Key[FirstNodeInfo]);
  WHILE AllSelected <> TRUE
  DO
    BEGIN
      InsertInTable(Node, TableOfNodes, CurrNode, I);
      GetNodeWithMinCost(MinCost, NodeMinCost, TableOfNodes);
      Node := FirstPtr;
      WHILE (Node^.Key[FirstNodeInfo] <> NodeMinCost) AND (Node^.Next <> NIL)
      DO
        Node := Node^.Next;
      INCLUDE(SetChosenNodes, Node^.Key[FirstNodeInfo]);
      CheckAllSelected(TableOfNodes, AllSelected);
      CurrNode := Node^.Key[FirstNodeInfo];
      IF NOT (Node^.Key[SecondNodeInfo] IN SetChosenNodes)
      THEN
        BEGIN
          I := Node^.Key[SecondNodeInfo];
        END;
      NodeMinCost := MAXINT;
      PrintTable(TableOfNodes);
    END;
  PrintMinimalOstovnyTree(TableOfNodes);
END;

BEGIN
  NodeCount := 0;
  CheckEnteredFile(FIn, FOut);
  ReadGraph(FIn);
  // GetOstovnyTree(FirstPtr);
END.
