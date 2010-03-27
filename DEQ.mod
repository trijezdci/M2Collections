(* M2DEQ
 *
 *  @file DEQ.mod
 *  M2DEQ interface
 *
 *  <description>
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


IMPLEMENTATION (* OF *) MODULE DEQ;

FROM SYSTEM IMPORT ADDRESS, ADR, TSIZE;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


TYPE

(* ---------------------------------------------------------------------------
 * private type:  ListEntry
 * ------------------------------------------------------------------------ *)
 
    ListPtr = POINTER TO ListEntry;

    ListEntry = RECORD
        value : DataPtr;
        prev,
        next  : ListPtr
    END; (* ListEntry *)


(* ---------------------------------------------------------------------------
 * type implementation DEQ.Queue
 * ------------------------------------------------------------------------ *)

    Queue = POINTER TO QueueDescriptor;
    
    QueueDescriptor = RECORD
        entryCount : QueueSize;
        head,
        tail       : ListPtr;
    END; (* QueueDescriptor *)


(* ---------------------------------------------------------------------------
 * function:  DEQ.new ( status )
 * ---------------------------------------------------------------------------
 *
 * Creates  and returns a new queue object.  Returns  NIL  if the queue object
 * could not be created.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE new ( VAR status : Status ) : Queue;

VAR
    newQueue : Queue;

BEGIN

    NEW(newQueue);
    
    (* bail out if allocation failed *)
    IF newQueue = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)

    (* initialise meta data *)
    newQueue^.entryCount := 0;
    newQueue^.head := NIL;
    newQueue^.tail := NIL;
    
    (* pass status and queue back to caller *)
    status := success;
    RETURN newQueue

END new;


(* ---------------------------------------------------------------------------
 * function:  DEQ.prepend( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Prepends  a new  entry <value>  at  the  head of <queue>.  The new entry is
 * added by reference,  no data is copied.  No entry is added if NIL is passed
 * in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE prepend ( queue : Queue; value : DataPtr; VAR status : Status );

VAR
    newEntry : ListPtr;

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

    NEW(newEntry);
    
    (* bail out if allocation failed *)
    IF newEntry = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)

    (* initialise new entry *)
    newEntry^.value := value;
    newEntry^.prev := NIL;
    
    (* check if queue is emptry *)
    IF queue^.entryCount = 0 THEN
        newEntry^.next := NIL;
        queue^.head := newEntry;
        queue^.tail := newEntry;
    ELSE (* not empty *)
        newEntry^.next := queue^.head;
        IF queue^.head # NIL THEN
            queue^.head^.prev := newEntry;
        END; (* IF *)
    END; (* IF *)
    
    (* update entry counter *)
    INC(queue^.entryCount);
    
    (* pass status to caller and return *)
    status := success;
    RETURN

END prepend;


(* ---------------------------------------------------------------------------
 * function:  DEQ.prepend( queue, value, status )
 * ---------------------------------------------------------------------------
 *
 * Appends  a  new  entry <value>  at  the  tail of <queue>.  The new entry is
 * added by reference,  no data is copied.  No entry is added if NIL is passed
 * in for <queue> or <value>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE append ( queue : Queue; value : DataPtr; VAR status : Status );

VAR
    newEntry : ListPtr;

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

    NEW(newEntry);
    
    (* bail out if allocation failed *)
    IF newEntry = NIL THEN
        status := allocationFailed;
        RETURN NIL;
    END; (* IF *)

    (* initialise new entry *)
    newEntry^.value := value;
    newEntry^.next := NIL;

    (* check if queue is emptry *)
    IF queue^.entryCount = 0 THEN
        newEntry^.prev := NIL;
        queue^.head := newEntry;
        queue^.tail := newEntry;
    ELSE (* not empty *)
        newEntry^.prev := queue^.tail;
        IF queue^.tail # NIL THEN
            queue^.tail^.next := newEntry;
        END; (* IF *)
    END; (* IF *)

    (* update entry counter *)
    INC(queue^.entryCount);
    
    (* pass status to caller and return *)
    status := success;
    RETURN
    
END append;


(* ---------------------------------------------------------------------------
 * function:  DEQ.firstEntry( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes  the first entry  from the head of <queue>  and returns it.  If the
 * queue is empty or NIL is passed in for <queue>,  then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE firstEntry ( queue : Queue; VAR status : Status ) : DataPtr;

VAR
    thisEntry : ListPtr;
    thisValue : DataPtr;

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
    
    (* remember first entry and its value *)
    thisEntry := queue^.head;
    thisValue := thisEntry^.value;
    
    (* unlink the entry from the queue *)
    queue^.head := queue^.head^.next;
    IF queue^.head # NIL THEN
        queue^.head^.prev := NIL;
    END; (* IF *)
    
    (* update entry counter *)
    DEC(queue^.entryCount);
    
    (* deallocate the entry *)
    DISPOSE(thisEntry);
    
    (* return value and status to caller *)
    success := success;
    RETURN thisValue;

END firstEntry;


(* ---------------------------------------------------------------------------
 * function:  DEQ.lastEntry( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Removes  the last entry  from the tail of <queue>  and  returns it.  If the
 * queue is empty or NIL is passed in for <queue>,  then NIL is returned.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE lastEntry ( queue : Queue; VAR status : Status ) : DataPtr;

VAR
    thisEntry : ListPtr;
    thisValue : DataPtr;

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
    
    (* remember last entry and its value *)
    thisEntry := queue^.tail;
    thisValue := thisEntry^.value;
    
    (* unlink the entry from the queue *)
    queue^.tail := queue^.tail^.prev;
    IF queue^.tail # NIL THEN
        queue^.tail^.next := NIL;
    END; (* IF *)
    
    (* update entry counter *)
    DEC(queue^.entryCount);
    
    (* deallocate the entry *)
    DISPOSE(thisEntry);
    
    (* return value and status to caller *)
    success := success;
    RETURN thisValue;

END lastEntry;


(* ---------------------------------------------------------------------------
 * function:  DEQ.capacity( queue )
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
    
    RETURN queue^.entryCount;
    
END capacity;


(* ---------------------------------------------------------------------------
 * function:  DEQ.entryCount( queue )
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
 * function:  DEQ.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN TRUE (* this is a dynamic queue implementation *)
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  DEQ.newIterator ( queue, status )
 * ---------------------------------------------------------------------------
 *
 * Creates and returns  a new iterator object  for iterating entries  in queue
 * <queue>.  Returns NIL if <queue> is NIL or empty.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE newIterator ( queue : Queue; VAR status : Status ) : Iterator;

BEGIN

    (* TO DO *)
    
    RETURN NIL
    
END newIterator;


(* ---------------------------------------------------------------------------
 * function:  DEQ.iterateNext( queue )
 * ---------------------------------------------------------------------------
 *
 * The  first  call  to this function  returns the  first  entry  of the queue
 * associated with <iterator>.  Subsequent calls return the queue's respective
 * successor entries.  Returns  NIL  if  any preceding call  returned the last
 * entry of the queue or if NIL is passed in for <iterator>.
 *
 * The status of the operation is passed back in <status>. *)

PROCEDURE iterateNext ( iterator : Iterator; VAR statusStatus ) : DataPtr;

BEGIN

    (* TO DO *)
    
    RETURN NIL
    
END iterateNext;


(* ---------------------------------------------------------------------------
 * function:  DEQ.disposeIterator ( iterator )
 * ---------------------------------------------------------------------------
 *
 * Disposes of iterator object <iterator> and returns NIL. *)

PROCEDURE disposeIterator ( VAR iterator : Iterator ) : Iterator;

BEGIN

    (* TO DO *)
    
    RETURN NIL
    
END disposeIterator;


(* ---------------------------------------------------------------------------
 * function:  DEQ.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

VAR
    thisEntry : ListPtr;

BEGIN

    IF queue # NIL THEN
        
        (* deallocate all entries *)
        WHILE queue^.head # NIL DO
            thisEntry := queue^.head;
            queue^.head := thisEntry^.next;
            DISPOSE(thisEntry);
        END; (* WHILE *)
        
        (* deallocate queue *)
        DISPOSE(queue);
        
    END; (* IF *)
    
    RETURN NIL

END dispose;


END DEQ.
