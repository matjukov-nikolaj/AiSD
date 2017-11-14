UNIT Stack;

INTERFACE
  PROCEDURE Clear; 
  PROCEDURE PushChar(Ch: CHAR); 
  FUNCTION PopChar: CHAR; 
  FUNCTION TopChar: CHAR;
  PROCEDURE StackCharsInOut;
  FUNCTION IsEmpty: BOOLEAN;
  
  PROCEDURE ClearInt; 
  PROCEDURE PushInt(Num: INTEGER); 
  FUNCTION PopInt: INTEGER; 
  FUNCTION TopInt: INTEGER;
  PROCEDURE StackInOutInt;
  FUNCTION IsEmptyInt: BOOLEAN;
  
IMPLEMENTATION
    
  CONST
    STACK_CHAR_IS_FULL = 'Stack characters is full';
    STACK_CHAR_IS_EMPTY = 'Stack characters is empty';
    MaxAmount = 100;
    
    STACK_INT_IS_FULL = 'Stack int is full';
    STACK_INT_IS_EMPTY = 'Stack int is empty';
    MaxSize = 100;
  TYPE
    PtrStackItem = ^StackItem;
    StackItem = RECORD
                  Ch: CHAR;
                  Next: ^StackItem;
                END;
    PtrStackEl= ^StackEl;
    StackEl = RECORD
                  Num: INTEGER;
                  Next: ^StackEl;
                END;
  VAR
    Current: PtrStackItem;
    Amount: INTEGER;
    
    Curr: PtrStackEl;
    Counter: INTEGER;

  PROCEDURE Clear; 
  VAR             
    Old: PtrStackItem;
  BEGIN
    WHILE (Current <> NIL)
    DO
      BEGIN
        Old := Current;
        Current := Current^.Next;
        Dispose(Old);
      END;
    Amount := 0;
  END;
  
  PROCEDURE StackCharsInOut;
  VAR 
    Ptr: PtrStackItem;
    Ch: CHAR;
  BEGIN
    Ptr := Current;
    WHILE Ptr <> NIL
    DO
      BEGIN
        WRITE(Ptr^.Ch, ' => ');
        Ptr := Ptr^.Next
      END;
    WRITELN
  END;
  
  PROCEDURE PushCh(Ch: CHAR); 
  VAR                       
    Old: PtrStackItem;
  BEGIN
    IF(Amount < MaxAmount)
    THEN
      BEGIN
        Old := Current;
        NEW(Current);
        Current^.Next := Old;
        Current^.Ch := Ch;
        INC(Amount);
      END;
  END;

  FUNCTION PopCh: CHAR;  
  VAR          
    Old: PtrStackItem;
    Ch: CHAR;
  BEGIN
    IF(Amount > 0)
    THEN
      BEGIN
        Ch := Current^.Ch;
        Old := Current;
        Current := Current^.Next;
        Dispose(Old);
        DEC(Amount);
        PopCh := Ch;
      END;
  END;
  
  FUNCTION TopCh: CHAR;
  VAR 
    Ch: CHAR;
  BEGIN
    IF(Amount > 0)
    THEN
      Ch := Current^.Ch;
    TopCh := Ch;
  END;

  FUNCTION IsEmpty: BOOLEAN;
  BEGIN
    IF (Amount = 0)
    THEN
      IsEmpty := TRUE
    ELSE
      IsEmpty := FALSE
  END;

  FUNCTION IsFull: BOOLEAN;
  BEGIN
    IF (Amount = MaxAmount)
    THEN
      IsFull := TRUE
    ELSE
      IsFull := FALSE
  END;
  
  PROCEDURE PushChar(Ch: CHAR);
  BEGIN
    IF(NOT(IsFull))
    THEN
      PushCh(Ch)
    ELSE
      WRITELN(STACK_CHAR_IS_FULL);
  END;

  FUNCTION PopChar: CHAR;
  BEGIN
    IF(NOT(IsEmpty))
    THEN
      PopChar := PopCh
    ELSE
      BEGIN
        PopChar := '@';
        WRITELN(STACK_CHAR_IS_EMPTY);
      END;
  END;

  FUNCTION TopChar: CHAR;
  BEGIN
    IF(NOT(IsEmpty))
    THEN
      TopChar := TopCh;
  END;
  
    PROCEDURE ClearInt; 
  VAR             
    Old: PtrStackEl;
  BEGIN
    WHILE (Curr <> NIL)
    DO
      BEGIN
        Old := Curr;
        Curr := Curr^.Next;
        Dispose(Old);
      END;
    Counter := 0;
  END;
  
  PROCEDURE StackInOutInt;
  VAR 
    Ptr: PtrStackEl;
    Num: INTEGER;
  BEGIN
    Ptr := Curr;
    WHILE Ptr <> NIL
    DO
      BEGIN
        WRITE(Ptr^.Num, '->');
        Ptr := Ptr^.Next
      END;
    WRITELN
  END;
  
  PROCEDURE PushEl(Num: INTEGER); 
  VAR                       
    Old: PtrStackEl;
  BEGIN
    IF(Counter < MaxSize)
    THEN
      BEGIN
        Old := Curr;
        NEW(Curr);
        Curr^.Next := Old;
        Curr^.Num := Num;
        INC(Counter);
      END;
  END;

  FUNCTION PopEl: INTEGER;  
  VAR          
    Old: PtrStackEl;
    Num: INTEGER;
  BEGIN
    IF(Counter > 0)
    THEN
      BEGIN
        Num := Curr^.Num;
        Old := Curr;
        Curr := Curr^.Next;
        Dispose(Old);
        DEC(Counter);
        PopEl := Num;
      END;
  END;
  
  FUNCTION TopEl: INTEGER;
  VAR 
    Num: INTEGER;
  BEGIN
    IF(Counter > 0)
    THEN
      Num := Curr^.Num;
    TopEl := Num;
  END;

  FUNCTION IsEmptyInt: BOOLEAN;
  BEGIN
    IF (Counter = 0)
    THEN
      IsEmptyInt := TRUE
    ELSE
      IsEmptyInt := FALSE
  END;

  FUNCTION IsFullInt: BOOLEAN;
  BEGIN
    IF (Counter = MaxSize)
    THEN
      IsFullInt := TRUE
    ELSE
      IsFullInt := FALSE
  END;
  
  PROCEDURE PushInt(Num: INTEGER);
  BEGIN
    IF(NOT(IsFullInt))
    THEN
      PushEl(Num)
    ELSE
      WRITELN(STACK_INT_IS_FULL);
  END;

  FUNCTION PopInt: INTEGER;
  BEGIN
    IF(NOT(IsEmptyInt))
    THEN
      PopInt := PopEl
    ELSE
      BEGIN
        PopInt := 0;
        WRITELN(STACK_INT_IS_EMPTY);
      END;
  END;

  FUNCTION TopInt: INTEGER;
  BEGIN
    IF(NOT(IsEmptyInt))
    THEN
      TopInt := TopEl;
  END;
  
BEGIN
END.
