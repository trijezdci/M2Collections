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


TYPE

(* ---------------------------------------------------------------------------
 * Private type: BinomialQueueNode
 * ------------------------------------------------------------------------ *)

    NodePtr = POINTER TO Node;

    BinomialQueueNode = RECORD
        value : DataPtr;
        left,
        right : NodePtr;
    END; (* BinomialQueueNode *)


(* ---------------------------------------------------------------------------
 * Type implementation:  PQ.Queue
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
END dequeue;


(* ---------------------------------------------------------------------------
 * function:  PQ.capacity( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of queue <queue>,  returns zero if NIL is passed
 * in for <queue>. *)

PROCEDURE capacity ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  PQ.entryCount( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in queue <queue>,  returns zero if NIL
 * is passed in for <queue>. *)

PROCEDURE entryCount ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  PQ.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN FALSE
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  PQ.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of priority queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

BEGIN
    (* TO DO *)
END dispose;


END PQ.
