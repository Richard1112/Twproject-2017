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
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Enumeration;
import java.util.Locale;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

//Logging Imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local Imports
import com.ericdaugherty.mail.external.PassReceivedLocalMessage;
import com.ericdaugherty.mail.external.PassReceivedLocalMessageImpl;

/**
 * The module control is responsible for instantiating the classes
 * supplied in directory external to execute tasks within the mail handling
 * process not connected to the core mailing activities.
 *
 * @author Andreas Kyrmegalos
 */
public class ModuleControl {

   //private static Log log = LogFactory.getLog(ModuleControl.class);
   private static Log log = LogFactory.getLog("JESLogger");
   
   private static ModuleControl moduleControl;
   private static PassReceivedLocalMessage passReceivedLocalMessage;

   public static PassReceivedLocalMessage getPassReceivedLocalMessage() {
      return passReceivedLocalMessage;
   }
   
   private ModuleControl() {}
   
   public static void shutdown() {
      if (passReceivedLocalMessage!=null) passReceivedLocalMessage.shutdown();
   }
   
   public static void initialize(String directory) {
      if (moduleControl==null) {
         moduleControl = new ModuleControl();
         passReceivedLocalMessage = new PassReceivedLocalMessageImpl();
         File externalDir = new File(directory, "external");
         if (!externalDir.exists()) {
            ConfigurationManager.getInstance().requestDirCreation("external");
            return;
         }
         File[] files = externalDir.listFiles();
         int length = files.length;
         if (length == 0) {
            passReceivedLocalMessage = new PassReceivedLocalMessageImpl();
         }
         else {
            URL[] urls = new URL[100];
            String filename;
            int trueFileCount = 0;
            Locale locale = Locale.getDefault();
            for (int i=0;i<length;i++) {
               if (!files[i].isDirectory()) {
                  filename = files[i].getName();
                  if (filename.toUpperCase(locale).indexOf("JAR")!=-1) {
                     try {
                        urls[trueFileCount++] = new URL("file", null, -1, files[i].getPath(), null);
                     } catch (MalformedURLException ex) {
                     }
                  }
               }
            }
            URL[] trueURLS = new URL[trueFileCount];
            System.arraycopy(urls, 0, trueURLS, 0, trueFileCount);
            ClassLoader classLoader = ModuleControl.class.getClassLoader();
            for (int i=0;i<length;i++) {
               if (!files[i].isDirectory()) {
                  filename = files[i].getName();
                  if (filename.toUpperCase(locale).indexOf("JAR")!=-1) {
                     JarFile jarFile = null;
                     String aJarEntryFileName;
                     try {
                        jarFile = new JarFile(files[i]);
                        Enumeration enumeration = jarFile.entries();
                        JarEntry jarEntry;
                        while(enumeration.hasMoreElements()) {
                           jarEntry = (JarEntry)enumeration.nextElement();
                           if (!jarEntry.isDirectory()) {
                              aJarEntryFileName = jarEntry.getName();
                              if (aJarEntryFileName.toUpperCase(locale).indexOf("CLASS")!=-1) {
                                 aJarEntryFileName = aJarEntryFileName.substring(0,aJarEntryFileName.indexOf(".")).replaceAll("/", ".");
                                 Class aClass = null;
                                 try {
                                    aClass = Class.forName(aJarEntryFileName, false, classLoader);
                                    Field[] f = aClass.getFields();
                                    for(int j=0;j<f.length;j++) {
                                       if (f[j].getName().equals("moduleType")) {
                                          try {
                                             if (f[j].get(null).toString().equals(PASS_MESSAGE_MODULE)) {
                                                if (log.isDebugEnabled()) {
                                                   log.debug("Instantiating PassReceivedLocalMessage");
                                                }
                                                try {
                                                   passReceivedLocalMessage = (PassReceivedLocalMessage)aClass.newInstance();
                                                }
                                                catch (Exception e) {
                                                   log.error("Error instantiating class ",e);
                                                }
                                                break;
                                             }
                                          }
                                          catch (IllegalArgumentException ex) {}
                                          catch (IllegalAccessException ex) {}
                                       }
                                    }
                                 } catch (ClassNotFoundException e) {
                                 }
                              }
                           }
                        }
                     }
                     catch (IOException e) {
                     }
                  }
               }
            }
         }
      }
   }
   
   private static final String PASS_MESSAGE_MODULE = "PassReceivedLocalMessage";

}
