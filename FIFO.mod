(* Modula-2 Collections Library
 *
 *  @file FIFO.mod
 *  Queue implementation
 *
 *  Universal Queue
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


IMPLEMENTATION (* OF *) MODULE FIFO;

FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * Opaque type : FIFO.Queue
 * ------------------------------------------------------------------------ *)

    Queue = POINTER TO QueueDescriptor;

    QueueDescriptor = RECORD
        size,
        entryCount,
        head,
        tail,      : QueueSize;
        array      : ADDRESS (* ARRAY OF DataPtr *)
    END; (* QueueDescriptor *)


(* ---------------------------------------------------------------------------
 * function:  FIFO.new ( size, status )
 * ---------------------------------------------------------------------------
 *
 * Creates and returns  a new queue object  with a storage capacity of <size>.
 * If zero is passed in for <size>,  then the new queue object will be created
 * with a capacity of  FIFO.defaultQueueSize.  Returns NIL if the queue object
 * could not be created.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( size : Capacity; VAR status : Status ) : Queue;

VAR
    newQueue : Queue;

BEGIN

    (* zero size means default *)
    IF size = 0 THEN
        size := defaultCapacity;
    END; (* IF *)
    
    (* bail out if size is too high *)
    IF size > maximumCapacity THEN
        status := invalidCapacity;
        RETURN NIL;
    END; (* IF *)
    
    (* allocate new queue object *)
    ALLOCATE(newQueue, TSIZE(Queue) + TSIZE(DataPtr) * (size - 1));
    
    (* bail out if allocation failed *)
    IF newQueue = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)
        
    (* initialise meta data *)
    newQueue^.size := size;
    newQueue^.entryCount := 0;
    newQueue^.head := 0;
    newQueue^.tail := 0;
    newQueue^.array := NIL;

    (* pass status and new stack to caller *)
    status := success;
    RETURN newQueue
    
END new;


(* ---------------------------------------------------------------------------
 * function:  FIFO.enqueue( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Adds a new entry <value> to the head of queue <queue>  and returns <queue>.
 * The new entry  is added  by reference,  NO data is copied.  If the queue is
 * full,  then  NO  new entry is added to <queue>.  The function fails  if NIL
 * is passed in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE enqueue ( queue : Queue; value : DataPtr; VAR status : Status );

VAR
    valuePtr : POINTER TO DataPtr;

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
    IF queue^.entryCount >= queue^.size THEN
        status := queueFull;
        RETURN;
    END; (* IF *)

    (* store value at head index *)

    (* queue^.array[queue^.head] := value; *)
    valuePtr := ADR(queue^.array) + TSIZE(DataPtr) * queue^.head;
    valuePtr^ := value;
    
    (* update entry counter *)
    INC(stack^.entryCount);
    
    (* update head index *)
    INC(queue^.head);
    IF queue^.head >= queue^.size THEN
        queue^.head := 0;
    END; (* IF *)

    (* pass status to caller *)
    status := success;
    RETURN

END enqueue;


(* ---------------------------------------------------------------------------
 * function:  FIFO.dequeue( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes the oldest value from the tail of queue <queue> and returns it.  If
 * the queue is empty or if NIL is passed in for <queue>,  NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE dequeue ( queue : Queue; VAR status : Status ) : DataPtr;

VAR
    valuePtr : POINTER TO DataPtr;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN;
    END; (* IF *)
    
    (* bail out if queue is empty *)
    IF queue^.entryCount = 0 THEN
        status := queueEmpty;
        RETURN;
    END; (* IF *)
    
    (* queue^.array[queue^.tail] *)
    valuePtr := ADR(queue^.array) + TSIZE(DataPtr) * queue^.tail;

    (* update entry counter *)
    DEC(queue^.entryCount);
    
    (* update tail index *)
    INC(queue^.tail);
    IF queue^.tail >= queue^.size THEN
        queue^.tail := 0;
    END; (* IF *)
    

    (* pass status and dequeued entry to caller *)
    status := success;
    (* RETURN queue^.array[queue^.tail] *)
    RETURN valuePtr^
    
END dequeue;


(* ---------------------------------------------------------------------------
 * function:  FIFO.capacity( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of queue <queue>,  returns zero if NIL is passed
 * in for <queue>. *)

PROCEDURE capacity ( queue : Queue ) : Capacity;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN 0;
    END; (* IF *)
    
    (* pass capacity to caller *)
    RETURN queue^.size
    
END capacity;


(* ---------------------------------------------------------------------------
 * function:  FIFO.entryCount( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in queue <queue>,  returns zero if NIL
 * is passed in for <queue>. *)

PROCEDURE entryCount ( queue : Queue ) : Capacity;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN 0;
    END; (* IF *)
    
    (* pass entry count to caller *)
    RETURN queue^.entryCount
    
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  FIFO.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN FALSE; (* this is a static queue implementation *)
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  FIFO.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

BEGIN

    (* bail out if queue is NIL *)
    IF queue = NIL THEN
        status := invalidQueue;
        RETURN NIL;
    END; (* IF *)
    
    (* deallocate queue object and pass NIL to caller *)
    DEALLOCATE(queue, TSIZE(Queue) + TSIZE(DataPtr) * (queue^.size - 1));
    RETURN NIL

END dispose;


END FIFO.
