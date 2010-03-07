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
 * Private type : ListEntry
 * ------------------------------------------------------------------------ *)

    ListPtr = POINTER TO ListEntry;

    ListEntry = RECORD
        value : DataPtr;
        next  : ListPtr
    END; (* ListEntry *)


(* ---------------------------------------------------------------------------
 * Opaque type : FIFO.Queue
 * ------------------------------------------------------------------------ *)

    Queue = POINTER TO QueueDescriptor;

    QueueDescriptor = RECORD
        entryCount : QueueSize;
        head       : ListPtr;
        tail       : ListPtr
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
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

BEGIN
    (* TO DO *)
END dequeue;


(* ---------------------------------------------------------------------------
 * function:  FIFO.capacity( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the total capacity of queue <queue>,  returns zero if NIL is passed
 * in for <queue>. *)

PROCEDURE capacity ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END capacity;


(* ---------------------------------------------------------------------------
 * function:  FIFO.entryCount( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns the number of entries stored in queue <queue>,  returns zero if NIL
 * is passed in for <queue>. *)

PROCEDURE entryCount ( queue : Queue ) : Capacity;

BEGIN
    (* TO DO *)
END entryCount;


(* ---------------------------------------------------------------------------
 * function:  FIFO.isResizable( queue )
 * ---------------------------------------------------------------------------
 *
 * Returns TRUE  if the  capacity of <queue> can change after <queue> has been
 * instatiated,  returns FALSE otherwise. *)

PROCEDURE isResizable ( queue : Queue ) : BOOLEAN;

BEGIN
    RETURN FALSE;
END isResizable;


(* ---------------------------------------------------------------------------
 * function:  FIFO.dispose ( queue )
 * ---------------------------------------------------------------------------
 *
 * Disposes of queue object <queue> and returns NIL. *)

PROCEDURE dispose ( VAR queue : Queue ) : Queue;

BEGIN
    (* TO DO *)
END dispose;


END FIFO.
