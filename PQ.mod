(* Modula-2 Collections Library
 *
 *  @file PQ.mod
 *  PQ implementaton
 *
 *  Universal Priority Queue
 *
 *  Author: Benjamin Kowarsch
 *
 *  Copyright (C) 2010 Benjamin Kowarsch. All rights reserved.
 *
 *  License:
 *
 *  Redistribution  and  use  in source  and  binary forms,  with  or  without
 *  modification, are permitted provided that the following conditions are met
 *
 *  1) NO FEES may be charged for the provision of the software.  The software
 *     may  NOT  be published  on websites  that contain  advertising,  unless
 *     specific  prior  written  permission has been obtained.
 *
 *  2) Redistributions  of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *
 *  3) Redistributions  in binary form  must  reproduce  the  above  copyright
 *     notice,  this list of conditions  and  the following disclaimer  in the
 *     documentation and other materials provided with the distribution.
 *
 *  4) Neither the author's name nor the names of any contributors may be used
 *     to endorse  or  promote  products  derived  from this software  without
 *     specific prior written permission.
 *
 *  5) Where this list of conditions  or  the following disclaimer, in part or
 *     as a whole is overruled  or  nullified by applicable law, no permission
 *     is granted to use the software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT  SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE  FOR  ANY  DIRECT,  INDIRECT,  INCIDENTAL,  SPECIAL,  EXEMPLARY,  OR
 * CONSEQUENTIAL  DAMAGES  (INCLUDING,  BUT  NOT  LIMITED  TO,  PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE,  DATA,  OR PROFITS; OR BUSINESS
 * INTERRUPTION)  HOWEVER  CAUSED  AND ON ANY THEORY OF LIABILITY,  WHETHER IN
 * CONTRACT,  STRICT LIABILITY,  OR TORT  (INCLUDING NEGLIGENCE  OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *  
 *)


IMPLEMENTATION (* OF *) MODULE PQ;

FROM SYSTEM IMPORT ADR, ADDRESS, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * private type: Octet
 * ------------------------------------------------------------------------ *)

    Octet = [0 .. 255];


(* ---------------------------------------------------------------------------
 * private type: BinomialQueueNode
 * ------------------------------------------------------------------------ *)

    NodePtr = POINTER TO BinomialQueueNode;

    BinomialQueueNode = RECORD
        value : DataPtr;
        left,
        right : NodePtr;
    END; (* BinomialQueueNode *)


(* ---------------------------------------------------------------------------
 * type implementation:  PQ.Queue
 * ------------------------------------------------------------------------ *)

    Queue = POINTER TO QueueDescriptor;
    
    QueueDescriptor = RECORD
        capacity,
        entryCount : Capacity;
        handler    : ComparisonHandler;
        heapSize,
        heapCount  : Octet;
        heap       : ADDRESS (* ARRAY OF BinomialQueueNode *)
    END; (* QueueDescriptor *)


(* ---------------------------------------------------------------------------
 * function:  PQ.new ( size, handler, status )
 * ---------------------------------------------------------------------------
 *
 * Creates and returns a new priority queue object.  The capacity of the queue
 * depends on the value passed in for <size>.  If <size> is less than or equal
 * to PQ.defaultCapacity,  the capacity will be PQ.defaultCapacity.  If <size>
 * is greater than PQ.maximumCapacity or if memory could not be allocated then
 * no queue will be created  and NIL will be returned.  Otherwise the capacity
 * of the new queue will be equal to the value of <size>.
 *
 * The  capacity of a queue  is the number of entries it can hold.  The under-
 * lying implementation uses a binomial queue (a set of power-of-two heaps) as
 * its internal storage structure.  Whilst storage for entries  are  allocated
 * dynamically,  pointers for the  power-of-two heaps  are allocated  when the
 * queue is created.  The number of heap pointers is determined by the queue's
 * capacity using the formula:  heap count = log2 ( capacity + 1).
 *
 * The  priority  of entries to be added to the queue is determined by calling
 * the priority comparison handler  passed in for <handler>.  If NIL is passed
 * in for handler, no queue will be created and NIL will be returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( size       : Capacity;
                handler    : ComparisonHandler;
                VAR status : Status ) : Queue;
VAR
    newQueue : Queue;
    index,
    heapSize : Octet;

BEGIN

    (* bail out if comparison handler is NIL *)
    IF handler = NIL THEN
        status := invalidHandler;
        RETURN NIL;
    END; (* IF *)

    (* bail out if size exceeds maximum capacity *)
    IF size > maximumCapacity THEN
        status := invalidSize;
        RETURN NIL;
    END; (* IF *)

    IF size < defaultCapacity THEN
        size := defaultCapacity;
    END; (* IF *)
    
    (* determine the number of binomial queues required for desired capacity *)
    heapSize := rootNodeCountForCapacity(size);
    ALLOCATE(newQueue, TSIZE(QueueDescriptor) + heapSize * TSIZE(NodePtr));
    
    (* bail out if allocation failed *)
    IF newQueue = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)
    
    (* initialise new queue *)
    newQueue^.capacity := size;
    newQueue^.entryCount := 0;
    newQueue^.handler := handler;
    newQueue^.heapSize := heapSize;
    newQueue^.heapCount := 0;
    newQueue^.heap := NIL;
    
    (* initialise heap pointers *)
    index := 0;
    WHILE index < heapSize DO
        (* newQueue^.heap[index] := NIL; *)
        heapPtr := ADR(newQueue^.heap) + TZIZE(NodePtr) * index;
        heapPtr^ := NIL;
        INC(index);
    END; (* WHILE *)

    (* pass queue and status back to caller *)
    status := success;
    RETURN newQueue;
    
END new;


(* ---------------------------------------------------------------------------
 * function:  PQ.enqueue( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Adds a new entry <value> to priority queue <queue>.  The new entry is added
 * by reference, no data is copied.  No entry is added if the queue's capacity
 * is insufficient to hold the new entry,  if memory could not be allocated or
 * if NIL is passed in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE enqueue ( queue : Queue; value : DataPtr; VAR status : Status );

VAR
    newNode,
    carryNode,
    heapPtr   : NodePtr;
    index     : Octet;
    
BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN;
    END; (* IF *)

    (* bail out if value is NIL *)
    IF value = NIL THEN
        status := invalidData;
        RETURN;
    END; (* IF *)

    (* bail out if queue is full *)
    IF queue^.entryCount >= queue^.capacity THEN
        status := queueFull;
        RETURN;
    END; (* IF *)
    
    (* allocate new node *)
    NEW(newNode);
    
    (* bail out if allocation failed *)
    IF newNode = NIL THEN
        status := allocationFailed;
        RETURN;
    END; (* IF *)

    (* initialise new node *)
    newNode^.value := value;
    newNode^.left := NIL;
    newNode^.right := NIL;
    
    (* insert new node *)
    index := 0;
    carryNode := newNode;
    WHILE carryNode # NIL AND index < queue^.heapSize DO
        
        (* place carry node in root node at index if empty *)
        
        (* test queue^.heap[index] *)
        heapPtr := ADR(queue^.heap) + TZIZE(NodePtr) * index;
        IF heapPtr^ = NIL THEN
            heapPtr^ := carryNode;
            EXIT;
        END; (* IF *);
    
        IF queue^.handler(carryNode^.value, heapPtr^.value) THEN
            (* carry node has priority *)
            carryNode^.value := heapPtr^.value;
            heapPtr^.right := carryNode^.left;
            carryNode^.left := heapPtr;
        ELSE (* carry node does not have priority *)
            carryNode^.right := heapPtr^.left;
            heapPtr^.left := carryNode;
            carryNode := heapPtr;
        END; (* IF *)
        
        heapPtr := NIL;
        queue^.heapCount := index + 1;
    END; (* WHILE *)
    
    (* update entry count *)
    INC(queue^.entryCount);
    
    (* pass status back to caller and return *)
    status := success;
    RETURN;
    
END enqueue;


(* ---------------------------------------------------------------------------
 * function:  PQ.inspectNext( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Retrieves the entry with the highest priority from <queue>  and  returns it
 * without removing the entry.  If the queue is empty  or if  NIL is passed in
 * for <queue> then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE inspectNext ( queue : Queue; VAR status : Status ) : DataPtr;

VAR
    thisIndex           : Octet;
    thisNode,
    highestPriorityNode : NodePtr;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN NIL;
    END; (* IF *)

    (* bail out if queue is empty *)
    IF queue^.entryCount = 0 THEN
        status := queueEmpty;
        RETURN NIL;
    END; (* IF *)
    
    (* find first non-empty heap *)
    thisIndex := 0;
    thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
    WHILE thisNode = NIL AND thisIndex < queue^.heapCount DO
        INC(thisIndex);
        thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
    END; (* WHILE *)
    
    (* remember this index and its root node *)
    INC(thisIndex);
    highestPriorityNode := thisNode;
    
    (* find heap with highest priority root node *)
    WHILE thisIndex < queue^heapCount DO
    
        (* check this heap *)
        IF thisNode^ # NIL THEN
            (* if non-empty then compare root to highest priority node *)
            IF queue^.handler(thisNode, highestPriorityNode) THEN
                (* this root node becomes new highest priority node *)
                highestPriorityNode := thisNode;
            END; (* IF *)
        END; (* IF *)
        
        (* move to next heap *)
        INC(thisIndex);
        thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
        
    END; (* WHILE *)
    
    (* return value and pass status to caller *)
    status := success;
    RETURN highestPriorityNode^.value;
    
END inspectNext;


(* ---------------------------------------------------------------------------
 * function:  PQ.dequeue( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes  the entry with the highest priority  from <queue>  and returns it.
 * If the queue is empty or NIL is passed in for <queue> then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE dequeue ( queue : Queue; VAR status : Status ) : DataPtr;

VAR
    thisIndex           : Octet;
    thisNode,
    highestPriorityNode : NodePtr;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN NIL;
    END; (* IF *)

    (* bail out if queue is empty *)
    IF queue^.entryCount = 0 THEN
        status := queueEmpty;
        RETURN NIL;
    END; (* IF *)
    
    (* find first non-empty heap *)
    thisIndex := 0;
    thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
    WHILE thisNode = NIL AND thisIndex < queue^.heapCount DO
        INC(thisIndex);
        thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
    END; (* WHILE *)
    
    (* remember this index and its root node *)
    INC(thisIndex);
    highestPriorityNode := thisNode;
    
    (* find heap with highest priority root node *)
    WHILE thisIndex < queue^heapCount DO
    
        (* check this heap *)
        IF thisNode^ # NIL THEN
            (* if non-empty then compare root to highest priority node *)
            IF queue^.handler(thisNode, highestPriorityNode) THEN
                (* this root node becomes new highest priority node *)
                highestPriorityNode := thisNode;
            END; (* IF *)
        END; (* IF *)
        
        (* move to next heap *)
        INC(thisIndex);
        thisNode := ADR(queue^.heap) + thisIndex * TSIZE(NodePtr);
        
    END; (* WHILE *)
        
    (* remove node containing highest priority root from its heap *)
    
    (* get node's value and deallocate the node *)
    thisValue := highestPriorityNode^.value;
    DEALLOCATE(highestPriorityNode);
    
    (* TO DO *)
    
    (* make binomial queue from remaining nodes *)
    
    (* merge resulting queue back in *)
    
    (* update entry count *)
    DEC(queue^.entryCount);
    
    (* return value and pass status to caller *)
    status := success;
    RETURN thisValue;
    
END dequeue;


(* ---------------------------------------------------------------------------
 * function:  PQ.capacity( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of queue <queue>,  returns zero if NIL is passed
 * in for <queue>. *)

PROCEDURE capacity ( queue : Queue ) : Capacity;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        RETURN 0;
    END; (* IF *)

    RETURN queue^.capacity;
    
END capacity;


(* ---------------------------------------------------------------------------
 * function:  PQ.entryCount( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in queue <queue>,  returns zero if NIL
 * is passed in for <queue>. *)

PROCEDURE entryCount ( queue : Queue ) : Capacity;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        RETURN 0;
    END; (* IF *)

    RETURN queue^.entryCount;
    
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  PQ.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN FALSE; (* this is a fixed maximum capacity implementation *)
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  PQ.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of priority queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        RETURN NIL;
    END; (* IF *)
    
    (* TO DO *)
    
    (* deallocate all nodes *)
    
    (* deallocate queue descriptor*)
    DEALLOCATE(queue);
    
    RETURN NIL;
    
END dispose;


(* ---------------------------------------------------------------------------
 * private function:  rootNodeCountForCapacity ( capacity )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of heaps required for a queue capacity of <capacity>. *)

PROCEDURE rootNodeCountForCapacity ( capacity : Capacity ) : Octet;

VAR
    limit     : Capacity;
    nodeCount : Octet;

BEGIN

    limit := 1;
    nodeCount := 1;
    WHILE size >= limit DO
        limit := limit * 2; (* limit := SHL(limit); *)
        INC(nodeCount);
    END; (* WHILE *)
    
    RETURN nodeCount;
    
END rootNodeCountForCapacity;


END PQ.
