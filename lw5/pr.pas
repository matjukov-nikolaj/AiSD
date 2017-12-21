PROGRAM DeleteNodeFromAVLTree(INPUT, OUTPUT);
{      16. Составить программу удаления записи из сбалансированного 
бинарного дерева поиска (АВЛ-дерева) (12).
                 Матюков Николай Юрьевич ПС-21
}
USES
  CRT;
CONST
  NO_FILE = 'No input data';
  ERROR_OPEN_FILE = 'Error open file ';
  RootLvl = 1;
  MinusOne = -1;
  Zero = 0;
  One = 1;
  END_PROGRAM = ' END PROGRAM - SHIFT + TAB';
  TO_LEFT = ' GO TO LEFT - LEFT ARROW';
  TO_RIGHT = ' GO TO RIGHT - RIGHT ARROW';
  TO_ROOT = ' UP ARROW - GO TO ROOT NODE';
  DEL = ' DELETE NODE';
  
TYPE
  Pointer = ^Node;
  Node = RECORD
           Key: INTEGER;
           LHeight, RHeight: INTEGER;
           Lvl: INTEGER;
           IsSelection: BOOLEAN;
           Left, Right: Pointer;
         END;
  BalanceInfo = RECORD
                  LL, RR, RL, LR: BOOLEAN;
                  TempNodeOne, TempNodeTwo, Node: Pointer;
                END;
  
VAR 
  FIn, FOut: TEXT;
  Root: Pointer;
  Level: INTEGER;
  BalInfo: BalanceInfo;

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

PROCEDURE Insert(VAR Ptr: Pointer; Data: INTEGER);
BEGIN
  IF Ptr = NIL
  THEN
    BEGIN
      NEW(Ptr);
      Ptr^.Key := Data;
      Ptr^.Lvl := Level;
      Ptr^.Left := NIL;
      Ptr^.Right := NIL;
      Ptr^.IsSelection := FALSE;
    END
  ELSE
    IF Ptr^.Key > Data
    THEN
      BEGIN
        INC(Level);
        Insert(Ptr^.Left, Data);
      END
    ELSE
      BEGIN
        INC(Level);
        Insert(Ptr^.Right, Data);
      END;
END;

PROCEDURE GetBalanceFactor(Ptr: Pointer; VAR MaxLvl: INTEGER);
BEGIN
  IF Ptr <> NIL
  THEN
    BEGIN
      IF Ptr^.Lvl > MaxLvl
      THEN 
        MaxLvl := Ptr^.Lvl;
      GetBalanceFactor(Ptr^.Left, MaxLvl);
      GetBalanceFactor(Ptr^.Right, MaxLvl);
    END;  
END;

PROCEDURE CreateBalanceFactor(Ptr: Pointer);
VAR
  MaxLeft, MaxRight: INTEGER;
BEGIN
  IF Ptr <> NIL
  THEN
    BEGIN
      MaxLeft := Ptr^.Lvl;
      MaxRight := Ptr^.Lvl;
      GetBalanceFactor(Ptr^.Left, MaxLeft);
      GetBalanceFactor(Ptr^.Right, MaxRight);
      Ptr^.LHeight := MaxLeft - Ptr^.Lvl; 
      Ptr^.RHeight := MaxRight - Ptr^.Lvl;    
      CreateBalanceFactor(Ptr^.Left);
      CreateBalanceFactor(Ptr^.Right);
    END;  
END;

PROCEDURE ReadTree(VAR FIn: TEXT);
VAR
  CurrKey: INTEGER;
BEGIN
  WHILE NOT EOLN(FIn)
  DO
    BEGIN
      READ(FIn, CurrKey);
      Insert(Root, CurrKey);  
      Level := RootLvl;          
    END;
  CreateBalanceFactor(Root);
END;

PROCEDURE SaveTree(VAR FOut: TEXT; Ptr: Pointer);
VAR
  I: INTEGER;
BEGIN
  IF Ptr <> NIL
  THEN
    BEGIN
      FOR I := 1 TO Ptr^.Lvl DO WRITE(FOut, '+');
      WRITELN(FOut, Ptr^.Key);       
      SaveTree(FOut, Ptr^.Left);
      SaveTree(FOut, Ptr^.Right);
    END;
END;

PROCEDURE PrintTree(Ptr: Pointer);
VAR 
  I: INTEGER;
BEGIN
  IF Ptr <> NIL
  THEN
    BEGIN
      IF Ptr^.IsSelection 
      THEN
        WRITE(' -> ')
      ELSE
        WRITE('    ');
      FOR I := 1 TO Ptr^.Lvl DO WRITE('+');
      WRITELN(Ptr^.Key, ' LHeight = ', Ptr^.LHeight, ' RHeight = ', Ptr^.RHeight);       
      PrintTree(Ptr^.Left);
      PrintTree(Ptr^.Right);
    END;
END;

PROCEDURE OnPressedRight(VAR CurrNode: Pointer);
BEGIN
  IF (CurrNode^.Right <> NIL) AND (CurrNode^.Left <> NIL)
  OR (CurrNode^.Right <> NIL) AND (CurrNode^.Left = NIL)
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Right^.IsSelection := TRUE;
      CurrNode := CurrNode^.Right;
    END;    
END;

PROCEDURE OnPressedLeft(VAR CurrNode: Pointer);
BEGIN
  IF (CurrNode^.Right <> NIL) AND (CurrNode^.Left <> NIL)
  OR (CurrNode^.Right = NIL) AND (CurrNode^.Left <> NIL)
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Left^.IsSelection := TRUE;
      CurrNode := CurrNode^.Left;
    END;  
END;

PROCEDURE OnPressedUp(VAR CurrNode: Pointer);
BEGIN
  CurrNode^.IsSelection := FALSE;
  CurrNode := Root;
  CurrNode^.IsSelection := TRUE;
END;

PROCEDURE ChangeLvl(VAR Node: Pointer);
BEGIN
  IF Node <> NIL
  THEN
    BEGIN
      Level := Node^.Lvl;
      IF Node^.Left <> NIL
      THEN
        Node^.Left^.Lvl := Level + 1;
      IF Node^.Right <> NIL
      THEN
        Node^.Right^.Lvl := Level + 1;
      ChangeLvl(Node^.Left);
      ChangeLvl(Node^.Right);
    END;
END;

PROCEDURE FindFather(Root: Pointer; VAR Father, DelNode: Pointer);
VAR 
  Node: Pointer;
BEGIN
  Node := Root;
  IF Father = NIL THEN
  IF Node <> NIL
  THEN
    BEGIN
      IF (Node^.Left = DelNode) OR (Node^.Right = DelNode)
      THEN
        Father := Node;
      FindFather(Node^.Left, Father, DelNode);
      FindFather(Node^.Right, Father, DelNode);
    END;
END;

PROCEDURE OnPressedDel(VAR Root, CurrNode: Pointer);
VAR
  DelNode, Father: Pointer;  
  
PROCEDURE DeleteNode(VAR MostRight: Pointer);
BEGIN
  IF MostRight^.Right <> NIL
  THEN
    DeleteNode(MostRight^.Right)
  ELSE
    BEGIN
      DelNode^.Key := MostRight^.Key;
      DelNode := MostRight;
      MostRight := MostRight^.Left;
      DISPOSE(DelNode);     
    END;
END;

PROCEDURE ChangePointerFather(Root: Pointer; VAR Father, DelNode: Pointer);
BEGIN
  FindFather(Root, Father, DelNode);
  IF Father^.Left = DelNode THEN
  Father^.Left := NIL ELSE 
  Father^.Right := NIL;
END;

BEGIN
  Father := NIL;
  DelNode := CurrNode;
  IF (DelNode^.Right = NIL) AND (DelNode^.Left = NIL)
  THEN
    BEGIN
      IF DelNode <> Root
      THEN
        BEGIN
          ChangePointerFather(Root, Father, DelNode);
          DISPOSE(DelNode);
        END;    
    END
  ELSE
  IF DelNode^.Right = NIL
  THEN
    BEGIN
      CurrNode := DelNode^.Left;
      IF DelNode <> Root
      THEN
      BEGIN
      FindFather(Root, Father, DelNode);
      IF Father^.Left = DelNode THEN
      Father^.Left := CurrNode ELSE 
      Father^.Right := CurrNode;
      END ELSE
      BEGIN
      CurrNode^.Lvl := RootLvl;
      Root := CurrNode;
      END;
      DISPOSE(DelNode);
    END
  ELSE
    IF DelNode^.Left = NIL
    THEN
      BEGIN
        CurrNode := DelNode^.Right;
        IF DelNode <> ROOT
        THEN
        BEGIN
        FindFather(Root, Father, DelNode);
        IF Father^.Right = DelNode THEN
        Father^.Right := CurrNode ELSE 
        Father^.Left := CurrNode;
        END ELSE
        BEGIN
        CurrNode^.Lvl := RootLvl;
        Root := CurrNode;
        END;
        DISPOSE(DelNode);
      END
    ELSE
      BEGIN
        DelNode^.IsSelection := FALSE;
        DeleteNode(DelNode^.Left);
      END;
   ChangeLvl(Root);
END;

PROCEDURE RightRotation(VAR Root, Node: Pointer);
VAR
  TempNode: Pointer;
BEGIN
  TempNode := Node^.Left;
  BalInfo.Node := Node;
  BalInfo.TempNodeOne := TempNode;
  Node^.Left := TempNode^.Right;
  TempNode^.Right := Node;
  IF Node = Root
  THEN
    BEGIN
      TempNode^.Lvl := RootLvl;
      Root := TempNode;
    END;
  Node := TempNode;
END;

PROCEDURE LeftRotation(VAR Root, Node: Pointer);
VAR
  TempNode: Pointer;
BEGIN
  TempNode := Node^.Right;
  BalInfo.Node := Node;
  BalInfo.TempNodeOne := TempNode;
  Node^.Right := TempNode^.Left;
  TempNode^.Left := Node;
  IF Root = Node
  THEN
    BEGIN
      TempNode^.Lvl := RootLvl;
      Root := TempNode;
    END;
  Node := TempNode;
END;

PROCEDURE LeftRightRotation(VAR Root, Node: Pointer);
VAR
  TempNode1, TempNode2: Pointer;
BEGIN
  TempNode1 := Node^.Left;
  TempNode2 := TempNode1^.Right;
  BalInfo.Node := Node;
  BalInfo.TempNodeOne := TempNode1;
  BalInfo.TempNodeTwo := TempNode2;
  TempNode1^.Right := TempNode2^.Left;
  TempNode2^.Left := TempNode1;
  Node^.Left := TempNode2^.Right;
  TempNode2^.Right := Node;
  IF Node = Root
  THEN
    BEGIN
      TempNode2^.Lvl := RootLvl;
      Root := TempNode2;
    END;  
  Node := TempNode2; 
END;

PROCEDURE RightLeftRotation(VAR Root, Node: Pointer);
VAR
  TempNode1, TempNode2: Pointer;
BEGIN
  TempNode1 := Node^.Right;
  TempNode2 := TempNode1^.Left;
  BalInfo.Node := Node;
  BalInfo.TempNodeOne := TempNode1;
  BalInfo.TempNodeTwo := TempNode2;
  TempNode1^.Left := TempNode2^.Right;
  TempNode2^.Right := TempNode1;
  Node^.Right := TempNode2^.Left;
  TempNode2^.Left := Node;
  IF Node = Root
  THEN
    BEGIN
      TempNode2^.Lvl := RootLvl;
      Root := TempNode2;
    END;  
  Node := TempNode2; 
END;


PROCEDURE BalanceTree(VAR Root, Node: Pointer);
BEGIN
  IF Node <> NIL
  THEN 
    BEGIN
      BalanceTree(Root, Node^.Left);
      BalanceTree(Root, Node^.Right);
      IF (Node^.LHeight - Node^.RHeight = 2)
      THEN
        IF (Node^.Left^.LHeight >= Node^.Left^.RHeight)
        THEN
          BEGIN
            BalInfo.RR := TRUE;
            RightRotation(Root, Node)
          END
        ELSE BEGIN BalInfo.LR := TRUE; LeftRightRotation(Root, Node); END;  
      IF (Node^.LHeight - Node^.RHeight = -2)
      THEN
        IF Node^.Right^.LHeight >= Node^.Right^.RHeight
        THEN
          BEGIN
            BalInfo.RL := TRUE;
            RightLeftRotation(Root, Node)
          END
        ELSE BEGIN BalInfo.LL := TRUE; LeftRotation(Root, Node); END;
    END;
END;

PROCEDURE InitBalInfo;
BEGIN
  BalInfo.LL := FALSE;
  BalInfo.RR := FALSE;
  BalInfo.RL := FALSE;
  BalInfo.LR := FALSE;
  BalInfo.Node := NIL;
  BalInfo.TempNodeOne := NIL;
  BalInfo.TempNodeTwo := NIL;
END;

PROCEDURE WriteBalInfo;
BEGIN
  IF BalInfo.LL THEN
  WRITELN('LL Rotation with Node: ', BalInfo.Node^.Key, ', TempNode: ', BalInfo.TempNodeOne^.Key)
  ELSE IF BalInfo.RR THEN
  WRITELN('RR Rotation with Node: ', BalInfo.Node^.Key, ', TempNode: ', BalInfo.TempNodeOne^.Key)
  ELSE IF BalInfo.RL THEN
  WRITELN('RL Rotation with Node: ', BalInfo.Node^.Key, ', TempNodeOne: ', 
  BalInfo.TempNodeOne^.Key, ', TempNodeTwo: ', BalInfo.TempNodeTwo^.Key)
  ELSE  IF BalInfo.LR THEN
  WRITELN('LR Rotation with Node: ', BalInfo.Node^.Key, ', TempNodeOne: ', 
  BalInfo.TempNodeOne^.Key, ', TempNodeTwo: ', BalInfo.TempNodeTwo^.Key)
  ELSE WRITELN('No rotations');
END;

PROCEDURE WriteHelpBox;
BEGIN
  WRITELN(END_PROGRAM);
  WRITELN(TO_LEFT); 
  WRITELN(TO_RIGHT);
  WRITELN(TO_ROOT);
  WRITELN(DEL);
  WRITELN;
END;

PROCEDURE MakeBalance(VAR Root, CurrNode: Pointer);
BEGIN
  CurrNode^.IsSelection := FALSE;
  BalanceTree(Root, CurrNode);
  CurrNode := Root;
  CurrNode^.IsSelection := TRUE;
  ChangeLvl(Root);
  CreateBalanceFactor(Root);
END;

PROCEDURE TreeController(VAR Root: Pointer);
VAR
  KeyChar: CHAR;
  EndProgram, OnPressedDelete: BOOLEAN;
  CurrNode: Pointer; 
BEGIN
  EndProgram := FALSE;
  CurrNode := Root;
  OnPressedDelete := FALSE;
  CurrNode^.IsSelection := TRUE;
  ClrScr;
  WriteHelpBox;
  PrintTree(Root);
  WHILE NOT EndProgram
  DO
    BEGIN
      KeyChar := ReadKey;   
      CASE KeyChar OF                       
        #72: OnPressedUp(CurrNode);      
        #77: OnPressedRight(CurrNode);
        #75: OnPressedLeft(CurrNode);
        #83: 
          BEGIN
            OnPressedDel(Root, CurrNode);
            CurrNode := Root;
            CurrNode^.IsSelection := TRUE;
            OnPressedDelete := TRUE;
          END;
        #15: 
          BEGIN
            ClrScr;
            EndProgram := TRUE;
          END;
      END;
      IF OnPressedDelete
      THEN
        BEGIN
          CreateBalanceFactor(Root);
          OnPressedDelete := FALSE;
          IF (((CurrNode^.RHeight - CurrNode^.LHeight) <> MinusOne)
          OR ((CurrNode^.RHeight - CurrNode^.LHeight) <> Zero)
          OR ((CurrNode^.RHeight - CurrNode^.LHeight) <> One))
          THEN
            BEGIN
              MakeBalance(Root, CurrNode);
              IF (ABS(Root^.LHeight - Root^.RHeight) = 2)
              THEN
                BEGIN
                  MakeBalance(Root, CurrNode);
                END;
            END;
        END;
      ClrScr;
      WriteHelpBox;
      WriteBalInfo;
      InitBalInfo;
      PrintTree(Root);
    END;
END;

PROCEDURE Main;
BEGIN
  CheckEnteredFile(FIn, FOut);
  Root := NIL;
  Level := RootLvl;
  ReadTree(FIn);
  Level := 0;
  TreeController(Root);
  SaveTree(FOut, Root);
  CLOSE(FOut);
  ClrScr;
END;

BEGIN
  Main;
END.
