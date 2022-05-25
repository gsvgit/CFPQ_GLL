module CFPQ_GLL.BTree

[<Struct>]
type BTree4Node<'value> =
    val mutable Filled: int
    
    val mutable Range0: Option<BTree4Node<'value>>
    val mutable Key0: int64
    val mutable Value0: Option<'value>
    
    val mutable Range1: Option<BTree4Node<'value>>
    val mutable Key1: int64
    val mutable Value1: Option<'value>
    
    val mutable Range2: Option<BTree4Node<'value>>
    val mutable Key2: int64
    val mutable Value2: Option<'value>
    
    val mutable Range3:Option<BTree4Node<'value>>
    
    new(range0, key, value, range1) =
        {
            Filled = 1
            
            Range0 = range0
            Key0 = key
            Value0 = Some value
            
            Range1 = range1
            Key1 = System.Int64.MaxValue
            Value1 = None
            
            Range2 = None
            Key2 = System.Int64.MaxValue
            Value2 = None
            
            Range3 = None
        }
        
    new(range0, key0, value0, range1, key1, value1, range2) =
        {
            Filled = 2
            
            Range0 = range0
            Key0 = key0
            Value0 = Some value0
            
            Range1 = range1
            Key1 = key1
            Value1 = Some value1
            
            Range2 = range2
            Key2 = System.Int64.MaxValue
            Value2 = None
            
            Range3 = None
        }

[<Struct>]
type InsertionResult<'value> =
    val Splitted : bool
    val Left: BTree4Node<'value>
    val Right: BTree4Node<'value>
    val Key: int64
    val Value: 'value
    
    new (splitted, left, right, key, value) =
        {
            Splitted = splitted
            Left = left
            Right = right
            Key = key
            Value = value
        }

    
type BTree<'value>(defaultValue) =
    let mutable storage = None
            
    let insert key value (leaf:BTree4Node<_>) left right =
        let mutable _localLeaf = leaf
        
        if key < _localLeaf.Key0
        then
            if _localLeaf.Filled = 2
            then
                _localLeaf.Key2 <- _localLeaf.Key1
                _localLeaf.Value2 <- _localLeaf.Value1
                _localLeaf.Range3 <- _localLeaf.Range2
                
                _localLeaf.Key1 <- _localLeaf.Key0
                _localLeaf.Value1 <- _localLeaf.Value0
                _localLeaf.Range2 <- _localLeaf.Range1
            elif _localLeaf.Filled = 1
            then
                _localLeaf.Key1 <- _localLeaf.Key0
                _localLeaf.Value1 <- _localLeaf.Value0
                _localLeaf.Range2 <- _localLeaf.Range1
                
            _localLeaf.Key0 <- key
            _localLeaf.Value0 <- Some value
            _localLeaf.Range0 <- left
            _localLeaf.Range1 <- right
        elif key < _localLeaf.Key1
        then
            if _localLeaf.Filled = 2
            then
                _localLeaf.Key2 <- _localLeaf.Key1
                _localLeaf.Value2 <- _localLeaf.Value1
                _localLeaf.Range3 <- _localLeaf.Range2
                
            _localLeaf.Key1 <- key
            _localLeaf.Value1 <- Some value
            _localLeaf.Range1 <- left
            _localLeaf.Range2 <- right
        else
            _localLeaf.Key2 <- key
            _localLeaf.Value2 <- Some value
            _localLeaf.Range2 <- left
            _localLeaf.Range3 <- right
        
        _localLeaf.Filled <- _localLeaf.Filled + 1
        _localLeaf

    
    let rec insertOrSplit key value (storage:BTree4Node<_>) left right =
        if storage.Filled < 3            
        then
            let insertionResult = insert key value storage left right
            InsertionResult(false, insertionResult, insertionResult, 0L, Unchecked.defaultof<_>) 
        elif key < storage.Key0
        then
            let newLeft = BTree4Node<_>(left, key, value, right, storage.Key0, storage.Value0.Value, storage.Range1)
            let newRight = BTree4Node<_>(storage.Range2, storage.Key2, storage.Value2.Value, storage.Range3)
            InsertionResult(true, newLeft, newRight, storage.Key1, storage.Value1.Value)
        elif key < storage.Key1
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0.Value, left, key, value, right)
            let newRight = BTree4Node<_>(storage.Range2, storage.Key2, storage.Value2.Value, storage.Range3)
            InsertionResult(true, newLeft, newRight, storage.Key1, storage.Value1.Value)
        elif key < storage.Key2
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0.Value, storage.Range1, storage.Key1, storage.Value1.Value, left)
            let newRight = BTree4Node<_>(right, storage.Key2, storage.Value2.Value, storage.Range3)
            InsertionResult(true, newLeft, newRight, key, value)
        else
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0.Value, storage.Range1, storage.Key1, storage.Value1.Value, storage.Range2)
            let newRight = BTree4Node<_>(left, key, value, right)
            InsertionResult(true, newLeft, newRight, storage.Key2, storage.Value2.Value)
                
    let rec findLeafAndInsert key value (storage:BTree4Node<_>) =
        match storage.Range0 with
        | None ->
            insertOrSplit key value storage None None
        | Some node ->
            let mutable _localNode = storage
            if key < storage.Key0
            then
                let insertionResult = findLeafAndInsert key value node
                if not insertionResult.Splitted  
                then
                    _localNode.Range0 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)           
            elif key < storage.Key1
            then
                let insertionResult = findLeafAndInsert key value storage.Range1.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range1 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)                
            elif key < storage.Key2
            then
                let insertionResult = findLeafAndInsert key value storage.Range2.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range2 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)                        
            else
                let insertionResult = findLeafAndInsert key value storage.Range3.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range3 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
                 
    member this.Add(key, value) =
        if key = System.Int64.MaxValue
        then failwith "Key can not be System.Int64.MinValue"
        
        match storage with 
        | None ->
            storage <- Some (BTree4Node<_>(None, key, value, None))
        | Some node ->
            let insertionResult = findLeafAndInsert key value node
            if not insertionResult.Splitted
            then storage <-  Some insertionResult.Left
            else storage <-  Some (BTree4Node<_>(Some insertionResult.Left, insertionResult.Key, insertionResult.Value, Some insertionResult.Right))
            
    member this.TryGetValue key =
        let rec find (storage:Option<BTree4Node<_>>) =
            match storage with
            | Some storage -> 
                if key = storage.Key0
                then storage.Value0
                elif key = storage.Key1
                then storage.Value1
                elif key = storage.Key2
                then storage.Value2
                elif key < storage.Key0
                then find storage.Range0
                elif key < storage.Key1
                then find storage.Range1
                elif key < storage.Key2
                then find storage.Range2
                else find storage.Range3
            | None -> None
            
        find storage