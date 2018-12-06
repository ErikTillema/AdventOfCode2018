module Problem5_ownLinkedListImplementation

    open System
    open Util

    type LinkedList<'a> = { mutable head: Link<'a> }
    and Link<'a> = 
        | EmptyLink
        | ValueLink of ValueLink<'a>
    and ValueLink<'a> = {   value: 'a;
                            mutable previous: Link<'a>;
                            mutable next:     Link<'a>;  }

    /// Removes the given link from the given list.
    let removeLink link list = 
        match link with
        | EmptyLink -> 
            invalidOp "can't remove EmptyLink"
        | ValueLink ({ previous = prev; next = next; value = _; }) ->
            match prev, next with
            | EmptyLink, EmptyLink ->                // [b]         -> EmptyLink
                list.head <- EmptyLink
            | EmptyLink, ValueLink(link) ->          // [b] - c     -> c
                link.previous <- EmptyLink
                list.head <- next
            | ValueLink(link), EmptyLink ->          // a - [b]     -> a
                link.next <- EmptyLink
            | ValueLink(link1), ValueLink(link2) ->  // a - [b] - c -> a - c
                link1.next <- next
                link2.previous <- prev

    /// Adds the value to the head of the list
    let addFirst value list = 
        let newHead = ValueLink({ previous = EmptyLink; next = list.head; value = value; })
        match list.head with
        | EmptyLink -> ()
        | ValueLink(l) -> l.previous <- newHead
        list.head <- newHead

    let getPrevious link = 
        match link with
        | EmptyLink -> invalidOp "expected a ValueLink"
        | ValueLink(valueLink) -> valueLink.previous

    let getNext link = 
        match link with
        | EmptyLink -> invalidOp "expected a ValueLink"
        | ValueLink(valueLink) -> valueLink.next

    let length list = 
        let rec length' acc list = 
            match list with
            | EmptyLink -> acc
            | ValueLink({previous=_; next=next; value=_}) -> length' (1+acc) next
        length' 0 list.head

    let toString (list: LinkedList<char>) = 
        let rec toString' acc list = 
            match list with
            | EmptyLink -> acc |> List.rev |> List.toArray |> String
            | ValueLink({previous=_; next=next; value=v}) -> toString' (v::acc) next
        toString' [] list.head

    let invert (c: char) = 
        if 'a' <= c && c <= 'z' then
            char ((int c) - (int 'a') + (int 'A'))
        else 
            char ((int c) - (int 'A') + (int 'a'))

    let solve list = 
        let mutable cur = list.head

        let rec simplify() = 
            let rec findPair() = 
                match cur with 
                | EmptyLink -> None
                | ValueLink(curLink) -> 
                    match curLink.next with
                    | EmptyLink -> None
                    | ValueLink(nextLink) -> 
                        match nextLink with
                        | { next=_; previous=_; value = v } when v = invert curLink.value -> Some(1)
                        | _ -> 
                            cur <- curLink.next
                            findPair()
            
            match findPair() with
            | None -> ()
            | _ -> 
                let newCur = match getPrevious cur with  
                             | EmptyLink -> cur |> getNext |> getNext
                             | _ -> cur |> getPrevious
                removeLink (getNext cur) list
                removeLink cur list
                cur <- newCur
                simplify()

        simplify()
        length list

    let getList (skipChar: char option) input = 
        let list = { head = EmptyLink }
        let sc = Scanner(input, false)
        sc.Next().Value 
            |> Seq.rev 
            |> Seq.filter (fun c -> skipChar.IsNone || (c <> skipChar.Value && c <> invert skipChar.Value))
            |> Seq.iter (fun c -> addFirst c list)
        list
    
    let solveSilver input = 
        input |> getList None |> solve

    let solveGold input = 
        seq { 'a'..'z' } |> Seq.map (fun c -> input |> getList (Some(c)) |> solve) |> Seq.min
