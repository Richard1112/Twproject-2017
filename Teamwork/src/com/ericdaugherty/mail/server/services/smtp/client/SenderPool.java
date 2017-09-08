/******************************************************************************
 * This program is a 100% Java Email Server.
 ******************************************************************************
 * Copyright (c) 2001-2013, Eric Daugherty (http://www.ericdaugherty.com)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the copyright holder nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 ******************************************************************************
 * For current versions and more information, please visit:
 * http://javaemailserver.sf.net/
 *
 * or contact the author at:
 * andreaskyrmegalos@hotmail.com
 *
 ******************************************************************************
 * This program is based on the CSRMail project written by Calvin Smith.
 * http://crsemail.sourceforge.net/
 ******************************************************************************
 *
 * $Rev$
 * $Date$
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.services.smtp.client;

//Java Import
import java.util.concurrent.*;

/**
 * This class allows SMTPSender to deliver mail concurrently.
 * Since the mail sending process itself doesn't share resources multiple
 * instances are allowed without provision for synchronization issues.
 * However access to the queue is externally synchronized, to prevent
 * any issue with concurrent inserts or removals and especially problems
 * that may arise when the queue is forced to increase its size.
 * 
 * @author Andreas Kyrmegalos
 */
public class SenderPool {

   private final BlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>(200);
   final FastThread[] threadPool;
   volatile boolean active = true;
   
   /**
    * The single constructor of this class.
    * 
    * @param count The maximum number of allowed threads.
    */
   public SenderPool(int count){
      threadPool = new FastThread[count];
   }
   
   public final void init(String name) {
      int count = threadPool.length;
      while(count-->0) {
         threadPool[count] = new FastThread(count+1, name);
         threadPool[count].start();
      }
   }
   
   /**
    * 
    * @param target The Runnable target of a mail delivery process.
    */
   boolean enterqueue(Runnable target) {

      //Although the prudent thing to do is check if the runnable
      //was added to the queue, it is not neccessery since such a
      //failure simply means that the runnable linked to a specific
      //message is discarded and the corresponding message will be
      //picked up by the next iteration over the smtp message pool.
      return getQueue().offer(target);
   }

   /**
    * Retrieve the queue item count
    *
    * @return The number of runnables waiting to be executed
    */
   int getSenderPoolQueuedItemCount() {
      return getQueue().size();
   }

   public BlockingQueue<Runnable> getQueue() {
      return queue;
   }
   
   /**
    * A simple extension of the Thread class to allow it to function as a
    * member of a threadPool.
    */
   class FastThread extends Thread {
      
      public FastThread(int increment, String name) {
         super(name+""+increment);
      }

      @Override
      public void run() {
         
         Runnable next = null;
         while(active) {
            while (active && next == null) {
               
               try {
                  next = getQueue().poll(1000, milliseconds);
               } catch (InterruptedException ex) {}
            }
            if (!active) return;
            next.run();
            next = null;
         }
      }
      
   }

   private static final TimeUnit milliseconds = TimeUnit.MILLISECONDS;

}
