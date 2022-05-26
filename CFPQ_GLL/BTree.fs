module CFPQ_GLL.BTree

open System.Xml.Schema

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
    
    val mutable Range3: Option<BTree4Node<'value>>
    val mutable Key3: int64
    val mutable Value3: Option<'value>
    
    val mutable Range4: Option<BTree4Node<'value>>
    val mutable Key4: int64
    val mutable Value4: Option<'value>
    
    val mutable Range5: Option<BTree4Node<'value>>
    val mutable Key5: int64
    val mutable Value5: Option<'value>
    
    val mutable Range6: Option<BTree4Node<'value>>
    val mutable Key6: int64
    val mutable Value6: Option<'value>
    
    val mutable Range7: Option<BTree4Node<'value>>
    val mutable Key7: int64
    val mutable Value7: Option<'value>
    
    val mutable Range8:Option<BTree4Node<'value>>
    
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
            Key3 = System.Int64.MaxValue
            Value3 = None
            
            Range4 = None
            Key4 = System.Int64.MaxValue
            Value4 = None
            
            Range5 = None
            Key5 = System.Int64.MaxValue
            Value5 = None
            
            Range6 = None
            Key6 = System.Int64.MaxValue
            Value6 = None
            
            Range7 = None
            Key7 = System.Int64.MaxValue
            Value7 = None
            
            Range8 = None
        }
        
    new(range0, key0, value0, range1, key1, value1, range2, key2, value2, range3, key3, value3, range4)=
        {
            Filled = 4
            
            Range0 = range0
            Key0 = key0
            Value0 = value0
            
            Range1 = range1
            Key1 = key1
            Value1 = value1
            
            Range2 = range2
            Key2 = key2
            Value2 = value2
            
            Range3 = range3
            Key3 = key3
            Value3 = value3
            
            Range4 = range4
            Key4 = System.Int64.MaxValue
            Value4 = None
            
            Range5 = None
            Key5 = System.Int64.MaxValue
            Value5 = None
            
            Range6 = None
            Key6 = System.Int64.MaxValue
            Value6 = None
            
            Range7 = None
            Key7 = System.Int64.MaxValue
            Value7 = None
            
            Range8 = None
        }

[<Struct>]
type InsertionResult<'value> =
    val Splitted : bool
    val Left: BTree4Node<'value>
    val Right: BTree4Node<'value>
    val Key: int64
    val Value: Option<'value>
    
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
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
            
            _localLeaf.Key5 <- _localLeaf.Key4
            _localLeaf.Value5 <- _localLeaf.Value4
            _localLeaf.Range6 <- _localLeaf.Range5
            
            _localLeaf.Key4 <- _localLeaf.Key3
            _localLeaf.Value4 <- _localLeaf.Value3
            _localLeaf.Range5 <- _localLeaf.Range4
            
            _localLeaf.Key3 <- _localLeaf.Key2
            _localLeaf.Value3 <- _localLeaf.Value2
            _localLeaf.Range4 <- _localLeaf.Range3
            
            _localLeaf.Key2 <- _localLeaf.Key1
            _localLeaf.Value2 <- _localLeaf.Value1
            _localLeaf.Range3 <- _localLeaf.Range2
            
            _localLeaf.Key1 <- _localLeaf.Key0
            _localLeaf.Value1 <- _localLeaf.Value0
            _localLeaf.Range2 <- _localLeaf.Range1            
                
            _localLeaf.Key0 <- key
            _localLeaf.Value0 <- value
            _localLeaf.Range0 <- left
            _localLeaf.Range1 <- right
        elif key < _localLeaf.Key1
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
            
            _localLeaf.Key5 <- _localLeaf.Key4
            _localLeaf.Value5 <- _localLeaf.Value4
            _localLeaf.Range6 <- _localLeaf.Range5
            
            _localLeaf.Key4 <- _localLeaf.Key3
            _localLeaf.Value4 <- _localLeaf.Value3
            _localLeaf.Range5 <- _localLeaf.Range4
            
            _localLeaf.Key3 <- _localLeaf.Key2
            _localLeaf.Value3 <- _localLeaf.Value2
            _localLeaf.Range4 <- _localLeaf.Range3
            
            _localLeaf.Key2 <- _localLeaf.Key1
            _localLeaf.Value2 <- _localLeaf.Value1
            _localLeaf.Range3 <- _localLeaf.Range2
                
            _localLeaf.Key1 <- key
            _localLeaf.Value1 <- value
            _localLeaf.Range1 <- left
            _localLeaf.Range2 <- right
        elif key < _localLeaf.Key2
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
            
            _localLeaf.Key5 <- _localLeaf.Key4
            _localLeaf.Value5 <- _localLeaf.Value4
            _localLeaf.Range6 <- _localLeaf.Range5
            
            _localLeaf.Key4 <- _localLeaf.Key3
            _localLeaf.Value4 <- _localLeaf.Value3
            _localLeaf.Range5 <- _localLeaf.Range4
            
            _localLeaf.Key3 <- _localLeaf.Key2
            _localLeaf.Value3 <- _localLeaf.Value2
            _localLeaf.Range4 <- _localLeaf.Range3
                                        
            _localLeaf.Key2 <- key
            _localLeaf.Value2 <- value
            _localLeaf.Range2 <- left
            _localLeaf.Range3 <- right
        elif key < _localLeaf.Key3
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
            
            _localLeaf.Key5 <- _localLeaf.Key4
            _localLeaf.Value5 <- _localLeaf.Value4
            _localLeaf.Range6 <- _localLeaf.Range5
            
            _localLeaf.Key4 <- _localLeaf.Key3
            _localLeaf.Value4 <- _localLeaf.Value3
            _localLeaf.Range5 <- _localLeaf.Range4                        
                                        
            _localLeaf.Key3 <- key
            _localLeaf.Value3 <- value
            _localLeaf.Range3 <- left
            _localLeaf.Range4 <- right
        elif key < _localLeaf.Key4
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
            
            _localLeaf.Key5 <- _localLeaf.Key4
            _localLeaf.Value5 <- _localLeaf.Value4
            _localLeaf.Range6 <- _localLeaf.Range5
                                        
            _localLeaf.Key4 <- key
            _localLeaf.Value4 <- value
            _localLeaf.Range4 <- left
            _localLeaf.Range5 <- right
        elif key < _localLeaf.Key5
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
            
            _localLeaf.Key6 <- _localLeaf.Key5
            _localLeaf.Value6 <- _localLeaf.Value5
            _localLeaf.Range7 <- _localLeaf.Range6
                                        
            _localLeaf.Key5 <- key
            _localLeaf.Value5 <- value
            _localLeaf.Range5 <- left
            _localLeaf.Range6 <- right
        elif key < _localLeaf.Key6
        then
            _localLeaf.Key7 <- _localLeaf.Key6
            _localLeaf.Value7 <- _localLeaf.Value6
            _localLeaf.Range8 <- _localLeaf.Range7
                                        
            _localLeaf.Key6 <- key
            _localLeaf.Value6 <- value
            _localLeaf.Range6 <- left
            _localLeaf.Range7 <- right
        else
            _localLeaf.Key7 <- key
            _localLeaf.Value7 <- value
            _localLeaf.Range7 <- left
            _localLeaf.Range8 <- right
        
        _localLeaf.Filled <- _localLeaf.Filled + 1
        _localLeaf

    
    let rec insertOrSplit key value (storage:BTree4Node<_>) left right =
        if storage.Filled < 8
        then
            let insertionResult = insert key value storage left right
            InsertionResult(false, insertionResult, insertionResult, 0L, Unchecked.defaultof<_>) 
        elif key < storage.Key0
        then
            let newLeft = BTree4Node<_>(left, key, value, right,
                                        storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3)
            let newRight = BTree4Node<_>(storage.Range4, storage.Key4, storage.Value4, storage.Range5,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key3, storage.Value3)
        elif key < storage.Key1
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, left,
                                        key, value, right,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3)
            let newRight = BTree4Node<_>(storage.Range4, storage.Key4, storage.Value4, storage.Range5,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key3, storage.Value3)
        elif key < storage.Key2
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, left,
                                        key, value, right,
                                        storage.Key2, storage.Value2, storage.Range3)
            let newRight = BTree4Node<_>(storage.Range4, storage.Key4, storage.Value4, storage.Range5,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key3, storage.Value3)
        elif key < storage.Key3
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, left,
                                        key, value, right)
            let newRight = BTree4Node<_>(storage.Range4, storage.Key4, storage.Value4, storage.Range5,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key3, storage.Value3)
        elif key < storage.Key4
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3,
                                        storage.Key3, storage.Value3, left)
            let newRight = BTree4Node<_>(right, storage.Key4, storage.Value4, storage.Range5,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, key, value)
        elif key < storage.Key5
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3,
                                        storage.Key3, storage.Value3, storage.Range4)
            let newRight = BTree4Node<_>(left, key, value, right,
                                         storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key4, storage.Value4)
        elif key < storage.Key6
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3,
                                        storage.Key3, storage.Value3, storage.Range4)
            let newRight = BTree4Node<_>(storage.Range5, storage.Key5, storage.Value5, left,
                                         key, value, right,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key4, storage.Value4)
        elif key < storage.Key7
        then
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3,
                                        storage.Key3, storage.Value3, storage.Range4)
            let newRight = BTree4Node<_>(storage.Range5, storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, left,
                                         key, value, right, 
                                         storage.Key7, storage.Value7, storage.Range8)
            InsertionResult(true, newLeft, newRight, storage.Key4, storage.Value4)
        else
            let newLeft = BTree4Node<_>(storage.Range0, storage.Key0, storage.Value0, storage.Range1,
                                        storage.Key1, storage.Value1, storage.Range2,
                                        storage.Key2, storage.Value2, storage.Range3,
                                        storage.Key3, storage.Value3, storage.Range4)
            let newRight = BTree4Node<_>(storage.Range5, storage.Key5, storage.Value5, storage.Range6,
                                         storage.Key6, storage.Value6, storage.Range7,
                                         storage.Key7, storage.Value7, left,
                                         key, value, right)
            InsertionResult(true, newLeft, newRight, storage.Key4, storage.Value4)
                
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
            elif key < storage.Key3
            then
                let insertionResult = findLeafAndInsert key value storage.Range3.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range3 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
            elif key < storage.Key4
            then
                let insertionResult = findLeafAndInsert key value storage.Range4.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range4 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
            elif key < storage.Key5
            then
                let insertionResult = findLeafAndInsert key value storage.Range5.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range5 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
            elif key < storage.Key6
            then
                let insertionResult = findLeafAndInsert key value storage.Range6.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range6 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
            elif key < storage.Key7
            then
                let insertionResult = findLeafAndInsert key value storage.Range7.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range7 <- Some insertionResult.Left
                    InsertionResult(false, _localNode, _localNode, 0L, Unchecked.defaultof<_>)
                else
                    insertOrSplit insertionResult.Key insertionResult.Value _localNode (Some insertionResult.Left) (Some insertionResult.Right)
            else
                let insertionResult = findLeafAndInsert key value storage.Range8.Value
                if not insertionResult.Splitted  
                then
                    _localNode.Range8 <- Some insertionResult.Left
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
            let insertionResult = findLeafAndInsert key (Some value) node
            if not insertionResult.Splitted
            then storage <-  Some insertionResult.Left
            else storage <-  Some (BTree4Node<_>(Some insertionResult.Left, insertionResult.Key, insertionResult.Value.Value, Some insertionResult.Right))
         
         
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
                elif key = storage.Key3
                then storage.Value3
                elif key = storage.Key4
                then storage.Value4
                elif key = storage.Key5
                then storage.Value5
                elif key = storage.Key6
                then storage.Value6
                elif key = storage.Key7
                then storage.Value7
                elif key < storage.Key0
                then find storage.Range0
                elif key < storage.Key1
                then find storage.Range1
                elif key < storage.Key2
                then find storage.Range2
                elif key < storage.Key3
                then find storage.Range3
                elif key < storage.Key4
                then find storage.Range4
                elif key < storage.Key5
                then find storage.Range5
                elif key < storage.Key6
                then find storage.Range6
                elif key < storage.Key7
                then find storage.Range7
                else find storage.Range8
            | None -> None
            
        find storage