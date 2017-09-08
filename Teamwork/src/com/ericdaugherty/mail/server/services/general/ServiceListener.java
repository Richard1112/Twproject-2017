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

package com.ericdaugherty.mail.server.services.general;

//Java imports
import java.net.*;
import java.io.*;
import java.util.concurrent.atomic.AtomicInteger;
import javax.net.ssl.*;

//Logging imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.configuration.ConfigurationManager;
import com.ericdaugherty.mail.server.services.pop3.Pop3Processor;
import com.ericdaugherty.mail.server.services.smtp.server.RFC5321SMTPServerSessionControl;

/**
 * This class listens for incoming connections on the specified port and
 * starts a new thread to process the request.  This class abstracts common
 * functionality required to start any type of service (POP3 or SMTP), reducing
 * the requirement to duplicate this code in each package.
 *
 * @author Eric Daugherty
 * @author Andreas Kyrrmegalos (2.x branch)
 */
public class ServiceListener implements Runnable {

    //***************************************************************
    // Variables
    //***************************************************************

    /** Logger Category for this class. */
    //private static Log log = LogFactory.getLog( ServiceListener.class );
  private static Log log = LogFactory.getLog("JESLogger");
    
    private static int totalSL;

    private static final Object lock = new Object();

    private static AtomicInteger ai;

    public static void setTotalSL(int totalSL) {
       ServiceListener.totalSL = totalSL;
       ai = new AtomicInteger();
    }

    public static Object getLock() {
       return lock;
    }

    public static boolean isSLsloadingComplete() {
       return ai.get()==totalSL;
    }

    private final ConfigurationManager cm = ConfigurationManager.getInstance();

    private boolean useSSL;
    
    private boolean delay;
    
    private boolean initialized = true;

    public boolean isInitialized() {
        return initialized;
    }

    /** Array of processors */
    private ConnectionProcessor[] processors;

    /** The port to listen on for incoming connections. */
    private int port;

    /** The type of class to use to handle requests. */
    private Class cpc;
    
    /** An indicator that affects generation of a new SMTPProcessor instance. */
    private boolean amavis;

    /** The number of threads to create to listen on this port. */
    private int threads;

    /** Thread pool */
    private Thread[] threadPool;

    /** server socket */
    private ServerSocket serverSocket;

    private static SSLServerSocketFactory sslServerSocketFactory;
    
    //***************************************************************
    // Public Interface
    //***************************************************************

    //***************************************************************
    // Constructor(s)

    /**
     * Creates a new instance and stores the initial parameters.
     */
    public ServiceListener( int port, Class cpc, int threads, boolean useSSL, boolean delay) {

       this.port = port;
       this.cpc = cpc;
       this.threads = threads;
       this.useSSL = useSSL;
       this.delay = delay;
       if (sslServerSocketFactory !=null) {
          //This constructor is never called from more than one Threads.
          sslServerSocketFactory = cm.getSSLServerSocketFactory();
       }
    }

    /**
     * Creates a new instance and stores the initial parameters.
     */
    public ServiceListener( int port, Class cpc, boolean amavis, int threads, boolean useSSL, boolean delay) {
       this(port, cpc, threads, useSSL, delay);
       this.amavis = amavis;
    }

    public int getPort() {
       return port;
    }

    //***************************************************************
    // Methods

    /**
     * Entry point for the thread.  Listens for incoming connections and
     * start a new handler thread for each.
     */
    public void run() {

        if( log.isDebugEnabled() )
          log.debug( "Starting ServiceListener on port: " + port );

        InetAddress listenAddress = cm.getListenAddress();
        try {
            serverSocket = setupServerSocket(listenAddress);
        }
        catch (IOException e) {
            String address = "localhost";
            if( listenAddress != null ) {
                address = listenAddress.getHostAddress();
            }

            log.error("Could not create ServiceListener on address: " + address + " port: " +port );
            if (!Mail.isStarted()) {
               initialized = false;
               if (ai.incrementAndGet()==totalSL) {
                  synchronized(lock) {
                     lock.notify();
                  }
               }
               return;
            }
            throw new RuntimeException(e.getMessage());
        }

        log.info( "Accepting Connections on port: " + port );

        ConnectionProcessor processor;
        long threadCount = 0;
        String threadNameBase = Thread.currentThread().getName();

        //Initialize threadpools.
        try {

            processors = new ConnectionProcessor[ threads ];
            threadPool = new Thread[ threads ];

            for( int index = 0; index < threads; index++ ) {
                //Create the handler now to speed up connection time.
                processor = (ConnectionProcessor) cpc.newInstance();
                processors[index] = processor;
                if (!processor.getClass().equals(Pop3Processor.class)) {
                   ((RFC5321SMTPServerSessionControl)processor).setUseAmavisSMTPDirectory(amavis);
                   ((RFC5321SMTPServerSessionControl)processor).setupVerifyIP();
                   if (delay) processor.setDelayedStart(true);
                }

                processor.setSocket( serverSocket );

                //Create, name, and start a new thread to handle this request.
                threadPool[index] = new Thread(processor, threadNameBase + ":" + ++threadCount );
                threadPool[index].start();
            }
        }
        catch (Exception e) {
            log.error("ServiceListener Connection failed on port: " + port + ".  Error: " + e );
            if (!Mail.isStarted()) {
               initialized = false;
            }
        }
        if (log.isDebugEnabled()) {
           log.debug("A ServiceListener instance completed initialization");
       }
        if (ai.incrementAndGet()==totalSL) {
           synchronized(lock) {
              lock.notify();
           }
        }
    }

   private ServerSocket setupServerSocket(InetAddress listenAddress) throws IOException{

      ServerSocket serverSocket;
      if (!useSSL || sslServerSocketFactory == null) {
         
         // 50 is the default backlog size.
          serverSocket = new ServerSocket( port, 50, listenAddress );
      }
      else {

         // 50 is the default backlog size.
         serverSocket = sslServerSocketFactory.createServerSocket(port, 50, listenAddress);
         if ("required".equals(cpc.equals(RFC5321SMTPServerSessionControl.class)?cm.getClientAuthSMTP():cm.getClientAuthPOP3())) {
           ((SSLServerSocket)serverSocket).setNeedClientAuth(true);
         }
         else if ("requested".equals(cpc.equals(RFC5321SMTPServerSessionControl.class)?cm.getClientAuthSMTP():cm.getClientAuthPOP3())) {
            ((SSLServerSocket)serverSocket).setWantClientAuth(true);
         }
         ((SSLServerSocket)serverSocket).
               setUseClientMode(false);
         ((SSLServerSocket)serverSocket).
               setEnabledCipherSuites(cm.getEnabledCiphers());
         ((SSLServerSocket)serverSocket).
               setEnabledProtocols(ConfigurationManager.getInstance().getEnabledProtocols());
      }
      try {
         serverSocket.setSoTimeout( 10 * 1000 );
         return serverSocket;
      }
      catch (SocketException e) {
         log.error("Error while trying to set server socket timeout");
         throw e;
      }
   }
    
    /**
     * This method notifies all processors to initiate shutdown.
     */
    public void notifyShutdown() {
       
        if (processors!=null) { 
           for( int index = 0; index < processors.length; index++ ) {

              if (processors[index]!=null) {
                  processors[index].shutdown();
              }
           }
        }
        try {
           if (serverSocket!=null) {
              serverSocket.close();
           }
        }
        catch(IOException ioe) {
           log.error(ioe.getMessage());
        }
    }

    /**
     * All processors are allowed to stop in sequence.
     */
    public void initiateShutdown() {
       if (processors==null) return;
        for( int index = 0; index < processors.length; index++ ) {

            try{
               if (threadPool[index]!=null) {
                   threadPool[index].join();
               }
            }
            catch (InterruptedException ie)
            {
                log.error("Was interrupted while waiting for thread to die");
            }
            threadPool[index] = null;
        }
    }
    
    public static void shutdown() {
        sslServerSocketFactory = null;
    }

    public Runnable updateServerSocket(final int port) {

       return new Runnable() {

          public void run() {
             InetAddress listenAddress = cm.getListenAddress();
             try {
                for( int index = 0; index < processors.length; index++ ) {
                   processors[index].setUpdatingServerSocket(true);
                }
                serverSocket.close();
                if (port!=-1) {
                   ServiceListener.this.port = port;
                }
                try {
                   Thread.sleep(500);
                }
                catch (InterruptedException e){
                  if (Mail.getInstance().isShuttingDown()) return;
                }
                serverSocket = setupServerSocket(listenAddress);
             }
             catch (IOException e) {
               String address = "localhost";
               if( listenAddress != null ) {
                   address = listenAddress.getHostAddress();
               }

               log.error("Could not create ServiceListener on address: " + address + " port: " + port +
                     ". Please select another address and/or port.");
               return;

             }
             for( int index = 0; index < processors.length; index++ ) {
                processors[index].setSocket(serverSocket);
                processors[index].setUpdatingServerSocket(false);
             }
          }
       };
    }
    
    public void start() {
       
       for( int index = 0; index < processors.length; index++ ) {
          processors[index].setDelayedStart(false);
       }
    }
}

