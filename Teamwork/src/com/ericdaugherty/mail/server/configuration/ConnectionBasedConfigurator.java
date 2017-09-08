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

package com.ericdaugherty.mail.server.configuration;

//Java Imports
import java.io.*;
import java.net.*;
import java.security.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import javax.net.ssl.*;

//Log imports
import org.apache.commons.logging.LogFactory;
import org.apache.commons.logging.Log;

//Local imports
import com.ericdaugherty.mail.server.Mail;

/**
 * Add a user and optionally an association with a (new) realm using
 * a custom communication protocol over a tcp connection.
 *
 * @author Andreas Kyrmegalos
 */
public class ConnectionBasedConfigurator implements Runnable {

   /** Logger Category for this class. */
   //private static final Log log = LogFactory.getLog(ConnectionBasedConfigurator.class);
   private static Log log = LogFactory.getLog("JESLogger");
   /** The server socket used to listen for incoming connections */
   private ServerSocket serverSocket;
   private static SSLServerSocketFactory sslServerSocketFactory = ConfigurationManager.getInstance().getSSLServerSocketFactory();
   /** Reader to read data from the client */
   private final ConfigurationManager cm = ConfigurationManager.getInstance();
   private volatile boolean shutdown;
   private final ThreadFactory tf;
   private final ExecutorService es;
   private final boolean secure;
   private final List<Socket> liveSockets = new ArrayList<Socket>();
   
   public void removeSocket(Socket socket) {
      if (shutdown) return;
      synchronized(this) {
         liveSockets.remove(socket);
      }
   }

   final class ExecutorThreadFactory implements ThreadFactory {

      private AtomicInteger ai = new AtomicInteger(1);
      private ThreadGroup tg;

      public ExecutorThreadFactory() {
         tg = new ThreadGroup("CBCGroup");
         tg.setMaxPriority(Thread.currentThread().getPriority() - 1);
         tg.setDaemon(false);
      }

      public Thread newThread(Runnable runnable) {
         return new Thread(tg, runnable, tg.getName() + "-" + ai.getAndIncrement());
      }
   }
   
   private AccessControlContext acc;

   public ConnectionBasedConfigurator(boolean secure) {

      this.secure = secure;
      PermissionCollection pc = new Permissions();
      InetAddress listenAddress = cm.getConfigurationAddress();
      if (listenAddress == null) {
         throw new RuntimeException("The CBC address can not be zero based");
      }
      pc.add(new SocketPermission("localhost:" + cm.getConfigurationPort(), "listen,resolve"));
      if (!secure) {
         pc.add(new SocketPermission(listenAddress.getHostAddress() + ":*", "accept,resolve"));
      }
      else {
         pc.add(new SocketPermission("*", "accept,resolve"));
      }
      acc = new AccessControlContext(new ProtectionDomain[]{new ProtectionDomain(null, pc)});

      tf = new ExecutorThreadFactory();
      es = Executors.newSingleThreadExecutor(tf);

      try {

         acc.checkPermission(new SocketPermission("localhost:" + cm.getConfigurationPort(), "listen,resolve"));
         if (!secure) {
            serverSocket = new ServerSocket(cm.getConfigurationPort(), 5, listenAddress);
         }
         else {
            serverSocket = sslServerSocketFactory.createServerSocket(cm.getConfigurationPort(), 5, listenAddress);
            ((SSLServerSocket)serverSocket).
                     setEnabledCipherSuites(ConfigurationManager.getInstance().getEnabledCiphers());
            ((SSLServerSocket)serverSocket).
                     setEnabledProtocols(ConfigurationManager.getInstance().getEnabledProtocols());
            ((SSLServerSocket)serverSocket).setNeedClientAuth(true);
         }

         log.info("CBC will be listening for changes.");
      } catch (IOException e) {

         log.error("Could not initiate ConnectionBasedConfigurator: "+e.getLocalizedMessage());
         Mail.getInstance().shutdown();
      }
   }

   public void shutdown() {
      log.warn("Remote Configurator going offline");
      shutdown = true;
      if (serverSocket != null) {
         try {
            serverSocket.close();
         } catch (IOException ex) {
         }
      }
      Iterator<Socket> iter = liveSockets.iterator();
      synchronized(this) {
         while(iter.hasNext()) {
            try {
               iter.next().close();
            } catch (IOException ex) {}
            iter.remove();
         }
      }
      
      es.shutdownNow();
   }

   public void run() {
      try {
         //Set the socket to timeout every 10 seconds so it does not
         //just block forever.
         serverSocket.setSoTimeout(10 * 1000);
      } catch (SocketException e) {
         log.error("Error initializing Socket Timeout in ConnectionBasedConfigurator");
         throw new RuntimeException("Error initializing Socket Timeout in ConnectionBasedConfigurator");
      }

      Socket socket;
      do {

         socket = null;
         try {
            socket = serverSocket.accept();
            acc.checkPermission(new SocketPermission(socket.getInetAddress().getHostAddress() + ":" + socket.getPort(), "accept,resolve"));
            socket.setSoTimeout(2 * 60 * 1000);
            log.info("Configuration Connection Established. Connecting client: "+socket.getInetAddress().getHostAddress() + ":" + socket.getPort());

            if (secure) {
               //Only applies to future connections attempting to use this session
               ((SSLSocket)socket).getSession().invalidate();
            }
            if (shutdown) {
               socket.close();
               return;
            }
            synchronized(this) {
               liveSockets.add(socket);
            }
            es.submit(new RunnableConfigCBC<Socket>(socket));
         } catch (SocketTimeoutException e) {
            //Do not log
         } catch (IOException e) {
            if (!shutdown) {
               log.error(e);
            }
         } catch (Exception e) {
            if (!shutdown) {
               log.error(e.getLocalizedMessage());
            }
         }
      } while (!shutdown);
   }
}
