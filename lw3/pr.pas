PROGRAM WorkWithFoldersInTree(INPUT, OUTPUT);
{   21. Информация  о  файлах  на  жестких  дисках  компьютера
записана  с  помощью  дерева.  Обеспечить выполнение следующих
операций:
   1) загрузку дерева в память из файла;
   2) обход дерева папок в  режиме  диалога  (раскрыиме папок,
      подъем на уровень и т. п.); 
   3) корректировку  дерева при создании новых папок и файлов,
их переименовании, копировании, переносе и удалении. 
   4) сохранение дерева в файле (13).

                 Матюков Николай Юрьевич ПС-21

   Инструкция эксплуатации:
   1) Стрелки движение по меню, стрелка вверх - движение 
      вверх, стрелка вниз - движение вниз, стрелка вправо -
      раскрытие папки, стрелка влево - возврат. 
      Выбранная папка или файл помечается стрелочкой ->.
   2) Создание новых папок - Клавиша TAB.
      Папка создается внутри выбранного узла.
   3) Удаление папки - клавиша Delete.
      ! Удалить диск - НЕЛЬЗЯ
   4) Переименование папки - клавиша F2.
      ! Переименовать букву диска - НЕЛЬЗЯ. 
   5) Вырезание папки для перемещения - Ctrl + X.
      ! Выезать диск и переместить - НЕЛЬЗЯ.
   6) Если мы вырезали папку, то для вставки нужно нажать Enter
      Папка вставится ниже выбранной папки.
   7) Сохранять файл - клавиша F10.    
   8) Выход из программы - SHIFT + TAB.
}
USES
  CRT;
CONST
  NO_FILE = 'Не указан исходный файл';
  ERROR_OPEN_FILE = 'Ошибка открытия файла ';
  LVL = '+';
  WRITE_NEW_NODE_NAME = 'Введите имя новой папки или файла: ';
  WRITE_NEW_NAME_FOR_NODE = 'Введите другое имя: ';
  MY_COMPUTER = 'Мой компьютер';
  ARROW = ' -> ';
  EMPTY_ARROW = '    ';
  SAVE_ALERT = 'Сохранение прошло успешно!';
  CONTROL = '  Управление - стрелки';
  NEW_FOLDER = '  Создание новой папки - TAB';
  DEL = '  Удаление - Delete';
  RENAME = '  Переименование - F2';
  CUT = '  Вырезание - Ctrl + X';
  COPY = '  Копирование - F4';
  INSERT = '  Вставка - Enter';
  SAVE = '  Сохранение - F10';
  EXIT = '  Выход - SHIFT + TAB';
  
TYPE
  Pointer = ^Node;
  Node = RECORD
           Val: STRING;
           Lvl: INTEGER;
           IsSelection: BOOLEAN;
           Left, Right, Prev, Father: Pointer;
         END;
VAR 
  FIn, FOut: TEXT;
  Tree, Root: Pointer;

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

PROCEDURE CreateRoot;
BEGIN
  NEW(Root);
  Root^.Val := MY_COMPUTER;
  Root^.Lvl := 0;
  Root^.Left := NIL;
  Root^.Right := NIL;
  Root^.Father := NIL;
END;

FUNCTION GetLevelCounter(CurrentValue: STRING): INTEGER;
VAR
  I, LvlCtr: INTEGER;
BEGIN
  LvlCtr := 0;
  I := 0;
  FOR I := 1 TO length(CurrentValue)
  DO
    BEGIN
      IF CurrentValue[I] = LVL
      THEN
        INC(LvlCtr)
      ELSE
        BREAK;
    END;
  GetLevelCounter := LvlCtr;
END;

FUNCTION GetNodeValue(CurrentValue: STRING): STRING;
VAR
  Val: STRING;
  I: INTEGER;
BEGIN
  I := 0;
  Val := '';
  FOR I := 1 TO length(CurrentValue)
  DO
    BEGIN
      IF CurrentValue[I] <> LVL
      THEN
        Val := Val + CurrentValue[I];
    END;
  GetNodeValue := Val;
END;

FUNCTION GetNewNode(CurrentValue: STRING; VAR LvlCtr: INTEGER): Pointer;
VAR
  NewNode: Pointer;
BEGIN
  NEW(NewNode);
  NewNode^.Val := CurrentValue;
  NewNode^.Lvl := LvlCtr;
  NewNode^.IsSelection := FALSE;
  NewNode^.Left := NIL;
  NewNode^.Right := NIL;
  GetNewNode := NewNode;
END;

PROCEDURE CreateNewLevel(VAR Node: Pointer; LvlCtr, LstLvlCtr: INTEGER);
BEGIN
  IF LvlCtr > LstLvlCtr
  THEN
    BEGIN
      Tree^.Left := Node;
      Node^.Prev := Tree;
      Node^.Father := Tree;
    END;
END;

PROCEDURE AddToCurrentLevel(VAR Node: Pointer; LvlCtr, LstLvlCtr: INTEGER);
BEGIN
  IF LvlCtr = LstLvlCtr
  THEN
    BEGIN
      Tree^.Right := Node;
      Node^.Prev := Tree;
      Node^.Father := Tree^.Father;
    END;
END;

PROCEDURE SearchLowerLevel(VAR Node: Pointer; LvlCtr, LstLvlCtr: INTEGER);
VAR
  Return: Pointer;
BEGIN
  IF LvlCtr < LstLvlCtr
  THEN
    BEGIN
      Return := Tree;
      WHILE Return^.Father^.Lvl <> LvlCtr - 1
      DO
        BEGIN
          Return := Return^.Father;
        END;
      Node^.Father := Return^.Father;
      Return^.Right := Node;
      Node^.Prev := Return;
    END;
END;

PROCEDURE ReadTree(VAR FIn: TEXT);
VAR
  LvlCounter, LastLvlCounter: INTEGER;
  Node: Pointer;
  CurrentValue: STRING;
BEGIN
  CreateRoot;
  Tree := Root;
  LastLvlCounter := 0;
  WHILE NOT EOF(FIn)
  DO
    BEGIN
      LvlCounter := 0;
      CurrentValue := '';
      READLN(FIn, CurrentValue);
      LvlCounter := GetLevelCounter(CurrentValue);
      CurrentValue := GetNodeValue(CurrentValue);
      Node := GetNewNode(CurrentValue, LvlCounter);
      CreateNewLevel(Node, LvlCounter, LastLvlCounter);
      AddToCurrentLevel(Node, LvlCounter, LastLvlCounter);
      SearchLowerLevel(Node, LvlCounter, LastLvlCounter);
      LastLvlCounter := LvlCounter; 
      Tree := Node; 
    END;  
END;

PROCEDURE SaveTree(VAR FOut: TEXT; Ptr: Pointer);
VAR
  I: INTEGER;
BEGIN   
  IF Ptr <> NIL
  THEN  
    BEGIN
      FOR I := 1 TO Ptr^.Lvl DO WRITE(FOut, LVL);
      WRITELN(FOut, Ptr^.Val);
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
      FOR I := 1 TO Ptr^.Lvl DO WRITE(LVL);
      WRITELN(Ptr^.Val);
      PrintTree(Ptr^.Left);
      PrintTree(Ptr^.Right);
    END;
END; 

PROCEDURE WriteMenuList(StartNode: Pointer);
VAR
  Node: Pointer;
BEGIN
  Node := StartNode;
  WHILE Node <> NIL
  DO
    BEGIN
      IF Node^.IsSelection
      THEN
        BEGIN
          WRITE(ARROW);
          WRITELN(Node^.Val);
        END         
      ELSE
        WRITELN(EMPTY_ARROW, Node^.Val);
      Node := Node^.Right;
    END;
END;

PROCEDURE WriteHelpBox();
BEGIN
  WRITELN(CONTROL);
  WRITELN(NEW_FOLDER);
  WRITELN(DEL);
  WRITELN(RENAME);
  WRITELN(CUT);
  WRITELN(COPY);
  WRITELN(INSERT);
  WRITELN(SAVE);
  WRITELN(EXIT);
  WRITELN;
END;

PROCEDURE UpdateMenuList(StartNode: Pointer);
VAR
  Node: Pointer;
BEGIN
  Node := StartNode;
  ClrScr;
  WriteHelpBox;
  WriteMenuList(Node);
END;

PROCEDURE OnPressedDown(VAR CurrNode: Pointer);
BEGIN
  IF CurrNode^.Right <> NIL
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Right^.IsSelection := TRUE;
      CurrNode := CurrNode^.Right;
    END;
END;

PROCEDURE OnPressedUp(VAR CurrNode: Pointer);
BEGIN
  IF CurrNode^.Prev <> CurrNode^.Father
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Prev^.IsSelection := TRUE;
      CurrNode := CurrNode^.Prev;
    END;  
END;

PROCEDURE OnPressedRight(VAR CurrNode, StartNode: Pointer);
BEGIN
  IF CurrNode^.Left <> NIL
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Left^.IsSelection := TRUE; 
      StartNode := CurrNode^.Left; 
      CurrNode := StartNode;
    END; 
END;

PROCEDURE OnPressedLeft(VAR CurrNode, StartNode: Pointer);
BEGIN
  IF CurrNode^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      CurrNode^.IsSelection := FALSE;
      CurrNode^.Father^.Father^.Left^.IsSelection := TRUE; 
      StartNode := CurrNode^.Father^.Father^.Left;
      CurrNode := StartNode;
    END;
END;

PROCEDURE OnPressedTab(VAR CurrNode: Pointer);
VAR
  NodeName: STRING;
  TempNode, NewNode: Pointer;
BEGIN
  WRITE(WRITE_NEW_NODE_NAME);
  READLN(NodeName);
  NewNode := GetNewNode(NodeName, CurrNode^.Lvl);
  TempNode := CurrNode;
  IF CurrNode^.Left <> NIL
  THEN
    BEGIN
      NewNode^.Lvl := CurrNode^.Lvl + 1;
      CurrNode := CurrNode^.Left;
      WHILE CurrNode^.Right <> NIL
      DO
        BEGIN
          CurrNode := CurrNode^.Right;
        END;
      CurrNode^.Right := NewNode;
      NewNode^.Prev := CurrNode;
      NewNode^.Father := CurrNode^.Father;
    END
  ELSE
    BEGIN
      NewNode^.Lvl := CurrNode^.Lvl + 1;
      CurrNode^.Left := NewNode;
      NewNode^.Prev := CurrNode;
      NewNode^.Father := CurrNode;
    END;
  CurrNode := TempNode;
END;

PROCEDURE RemovalChilds(VAR Node: Pointer);
BEGIN
  IF Node <> NIL
  THEN
    BEGIN
      RemovalChilds(Node^.Left);
      RemovalChilds(Node^.Right);
      DISPOSE(Node);
    END;
END;

PROCEDURE RemoveFirstChild(VAR Node, CurrNode, StartNode: Pointer);
BEGIN
  StartNode := Node^.Right;
  Node^.Right^.IsSelection := TRUE;
  Node^.Father^.Left := Node^.Right;
  Node^.Right^.Prev := Node^.Father;
  CurrNode := Node^.Right;
END;

PROCEDURE RemoveCenterChild(VAR Node, CurrNode: Pointer);
BEGIN
  Node^.Prev^.IsSelection := TRUE;
  Node^.Prev^.Right := Node^.Right;
  Node^.Right^.Prev := Node^.Prev;
  CurrNode := Node^.Prev;
END;

PROCEDURE RemoveLastChild(VAR Node, CurrNode: Pointer);
BEGIN
  Node^.Prev^.IsSelection := TRUE;
  Node^.Prev^.Right := NIL;
  CurrNode := Node^.Prev;
END;

PROCEDURE RemoveAloneChild(VAR Node, CurrNode, StartNode: Pointer);
BEGIN
  StartNode := Node^.Father^.Father^.Left;
  Node^.Father^.IsSelection := TRUE;
  Node^.Father^.Left := NIL;
  CurrNode := Node^.Father;
END;

PROCEDURE RemoveFatherNode(VAR Node, CurrNode, StartNode: Pointer);
BEGIN
  IF (Node^.Right <> NIL) AND (Node^.Prev <> Node^.Father)
  THEN
    RemoveCenterChild(Node, CurrNode);
  IF (Node^.Prev = Node^.Father) AND (Node^.Right <> NIL)
  THEN
    RemoveFirstChild(Node, CurrNode, StartNode);
  IF (Node^.Prev <> Node^.Father) AND (Node^.Right = NIL)
  THEN
    RemoveLastChild(Node, CurrNode);
  IF (Node^.Prev = Node^.Father) AND (Node^.Right = NIL)
  THEN
    RemoveAloneChild(Node, CurrNode, StartNode);
END;

PROCEDURE DeleteNode(VAR CurrNode, StartNode: Pointer);
VAR
  DelNode, Node: Pointer;
BEGIN
  Node := CurrNode;
  IF Node^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      DelNode := Node^.Left;
      RemovalChilds(DelNode);
      RemoveFatherNode(Node, CurrNode, StartNode);
      DISPOSE(Node);
    END;
END;

PROCEDURE OnPressedDel(VAR CurrNode, StartNode: Pointer);
BEGIN
  DeleteNode(CurrNode, StartNode);
END;

PROCEDURE OnPressedF2(VAR CurrNode: Pointer);
VAR
  NewName: STRING;
BEGIN
  IF CurrNode^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      WRITE(WRITE_NEW_NAME_FOR_NODE);
      READLN(NewName);
      CurrNode^.Val := NewName;
    END;
END;

FUNCTION OnPressedCtrlX(VAR CurrNode, StartNode: Pointer): Pointer;
VAR
  SaveNode: Pointer;
BEGIN
  IF CurrNode^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      SaveNode := CurrNode;
      RemoveFatherNode(SaveNode, CurrNode, StartNode);
      SaveNode^.Right := NIL;
      SaveNode^.Prev := NIL;
      SaveNode^.Father := NIL;
      SaveNode^.IsSelection := FALSE;
      OnPressedCtrlX := SaveNode;
    END;
END;

PROCEDURE ChangeLvl(VAR Ptr: Pointer);
BEGIN
  IF Ptr <> NIL 
  THEN  
    BEGIN
      IF (Ptr^.Left <> NIL) AND (Ptr^.Right <> NIL)
      THEN
      BEGIN
        Ptr^.Right^.Lvl := Ptr^.Lvl;
        Ptr^.Left^.Lvl := Ptr^.Lvl + 1;
      END;
      IF (Ptr^.Left = NIL) AND (Ptr^.Right <> NIL)
      THEN
        Ptr^.Right^.Lvl := Ptr^.Lvl;
      IF (Ptr^.Left <> NIL) AND (Ptr^.Right = NIL)
      THEN
        BEGIN
          Ptr^.Left^.Lvl := Ptr^.Lvl + 1;
        END;
      ChangeLvl(Ptr^.Left);
      ChangeLvl(Ptr^.Right);
    END;
END;

PROCEDURE InsertChild(VAR CutOutNode, CurrNode: Pointer);
BEGIN
  CutOutNode^.Lvl := CurrNode^.Lvl;
  ChangeLvl(CutOutNode); 
  CutOutNode^.Right := CurrNode^.Right;
  CutOutNode^.Prev := CurrNode;
  CurrNode^.Right^.Prev := CutOutNode;
  CutOutNode^.Father := CurrNode^.Father;
  CurrNode^.Right := CutOutNode;
END;

PROCEDURE InsertAfterLastChild(VAR CutOutNode, CurrNode: Pointer);
BEGIN
  CutOutNode^.Lvl := CurrNode^.Lvl;
  ChangeLvl(CutOutNode); 
  CurrNode^.Right := CutOutNode;
  CutOutNode^.Prev := CurrNode;
  CutOutNode^.Father := CurrNode^.Father;
  CutOutNode^.Lvl := CurrNode^.Lvl;
END;

PROCEDURE OnPressedEnter(VAR CutOutNode, CurrNode: Pointer);
BEGIN
  IF CurrNode^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      IF (CurrNode^.Right <> NIL)
      THEN
        InsertChild(CutOutNode, CurrNode)
      ELSE
        InsertAfterLastChild(CutOutNode, CurrNode);
    END;
END;

PROCEDURE MakeCopy(VAR CopyNode: Pointer; Ptr: Pointer);
VAR
  NewNode: Pointer;
BEGIN
  IF Ptr <> NIL
  THEN  
    BEGIN
      NEW(NewNode);
      NewNode := Ptr;
      CopyNode := NewNode;
      MakeCopy(CopyNode^.Left, Ptr^.Left);
      MakeCopy(CopyNode^.Right, Ptr^.Right);
    END;
END;

FUNCTION OnPressedF4(CurrNode: Pointer): Pointer;
VAR
  CopyNode: Pointer;
BEGIN
  IF CurrNode^.Father^.Val <> MY_COMPUTER
  THEN
    BEGIN
      NEW(CopyNode);
      CopyNode^.Val := CurrNode^.Val;
      CopyNode^.Right := NIL;
      CopyNode^.Left := NIL;
      CopyNode^.Prev := NIL;
      CopyNode^.Father := NIL;
      CopyNode^.Lvl := CurrNode^.Lvl;
      MakeCopy(CopyNode^.Left, CurrNode^.Left);
      OnPressedF4 := CopyNode;
    END;
END;

PROCEDURE TreeController(Root: Pointer);
VAR
  Key: CHAR;
  EndProgram, Cut, IsSave, Copy: BOOLEAN;
  CurrNode, StartNode, CutOutNode, CopyNode: Pointer;
BEGIN
  EndProgram := FALSE;
  Cut := FALSE;
  IsSave := FALSE;
  Copy := FALSE;
  IF Root^.Left <> NIL
  THEN
    BEGIN
      CurrNode := Root;
      CurrNode := CurrNode^.Left;
      CurrNode^.IsSelection := True;
      StartNode := CurrNode;
      UpdateMenuList(StartNode);
      WHILE NOT EndProgram 
      DO
        BEGIN
          Key := ReadKey;
          IF Key = #0 THEN Key := ReadKey;
          CASE Key OF
            #80: OnPressedDown(CurrNode);                        
            #72: OnPressedUp(CurrNode);      
            #77: OnPressedRight(CurrNode, StartNode);
            #75: OnPressedLeft(CurrNode, StartNode);
            #9 : OnPressedTab(CurrNode);
            #83: OnPressedDel(CurrNode, StartNode);
            #60: OnPressedF2(CurrNode);
            #62: 
              BEGIN 
                IF Copy = FALSE
                THEN
                  BEGIN
                    CopyNode := OnPressedF4(CurrNode);
                    Copy := TRUE;
                  END;
              END;
            #24:
              BEGIN
                IF Cut = FALSE
                THEN
                  BEGIN
                    CutOutNode := OnPressedCtrlX(CurrNode, StartNode);
                    Cut := TRUE;
                  END;
              END;
            #13: 
              BEGIN
                IF Copy
                THEN
                  BEGIN
                    OnPressedEnter(CopyNode, CurrNode);
                    Copy := FALSE;
                  END;             
                IF Cut
                THEN
                  BEGIN
                    OnPressedEnter(CutOutNode, CurrNode);
                    Cut := FALSE;
                  END;
              END;
            #68: 
              BEGIN
                IF IsSave = FALSE
                THEN
                  BEGIN
                    REWRITE(FOut);
                    SaveTree(FOut, Root^.Left);
                    CLOSE(FOut);
                    IsSave := TRUE;
                  END;
              END;         
            #15: 
              BEGIN
                ClrScr;
                EndProgram := TRUE;
              END;
          END;
          UpdateMenuList(StartNode);
          IF IsSave
          THEN
            BEGIN
              IsSave := FALSE;
              WRITELN(SAVE_ALERT);
            END;
        END;
    END;
END;

BEGIN
  CheckEnteredFile(FIn, FOut);
  ReadTree(FIn);
  Tree := Root;
  TreeController(Tree);
END.
