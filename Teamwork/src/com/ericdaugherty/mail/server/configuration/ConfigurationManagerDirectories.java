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

//Java imports
import java.io.*;
import java.util.*;
import org.w3c.dom.Element;

//Log imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.errors.UserCreationException;
import com.ericdaugherty.mail.server.info.Domain;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryFactory;
import com.ericdaugherty.mail.server.persistence.LocalDeliveryProcessor;

/**
 *
 * @author Andreas Kyrmegalos
 */
final class ConfigurationManagerDirectories implements ConfigurationParameterConstants {

   /** Logger */
   //private static Log log = LogFactory.getLog(ConfigurationManager.class);
   private static Log log = LogFactory.getLog("JESLogger");
   private final ConfigurationManager cm;

   Map<String, String> getConfiguration() {

      Map<String, String> configuration = new HashMap<String, String>();

      configuration.put("dirRoot", getRootDirectory());
      
      String dir = getSMTPDirectory();
      if(dir.equals(getRootDirectory()+File.separator+"smtp")) {
         dir = "using default SMTP directory";
      }
      configuration.put("dirSMTP", dir + RESTART);
      
      dir = getUsersDirectory();
      if(dir.equals(getRootDirectory()+File.separator+"users")) {
         dir = "using default users directory";
      }
      configuration.put("dirUsers", dir + RESTART);
      
      dir = getFailedDirectory();
      if(dir.equals(getRootDirectory()+File.separator+"failed")) {
         dir = "using default failed directory";
      }
      configuration.put("dirFailed", dir + RESTART);
      
      dir = getTestingDirectory();
      if (dir==null) {
         dir = "";
      }
      configuration.put("dirTesting", dir + RESTART);
      
      configuration.put("fileSeparator", File.separator);
      configuration.put("allowRemoteRestart", Boolean.toString(cm.isAllowRemoteRestart()));

      return configuration;
   }
   
   //Used for testing purposes only
   ConfigurationManagerDirectories(){
      
      cm = null;
   }

   ConfigurationManagerDirectories(final String rootDirectory) {
      
      cm = ConfigurationManager.getInstance();

      dirRequests = new ArrayList<String>(10);
      setRootDirectory(rootDirectory);
      setSecurityDirectory(rootDirectory + File.separator + "security");
      setBackupDirectory(rootDirectory + File.separator + "backup");
      setSMTPDirectory(rootDirectory + File.separator + "smtp");
      setUsersDirectory(rootDirectory + File.separator + "users");
      setFailedDirectory(rootDirectory + File.separator + "failed");
      System.setProperty("jes.install.directory", rootDirectory);
   }
   
   //TODO switch to a priority queue
   private List<String> dirRequests;

   public void requestDirCreation(String directory) {
      if (dirRequests.contains(directory)) {
         return;
      }
      if (log.isDebugEnabled()) {
         log.debug("adding request to create directory " + directory.substring(directory.lastIndexOf(File.separatorChar)+1));
      }
      dirRequests.add(directory);
   }
   
   public boolean checkLegacyFileIO(String usersDirectory, Set<? extends Domain> domains) {
      
      File usersDir = new File(usersDirectory);
      
      File[] files = usersDir.listFiles();
      
      Map<String, List<String>> domainDirs = new LinkedHashMap<String, List<String>>();
      String domain;
      List<String> userDirs;
      for (File file:files) {
         if (file.isDirectory()) {
            if (domains.contains(new Domain(file.getName()))) {
               throw new RuntimeException("Can not switch to new FileIO mode. The directory "
                     + file.getName() + " denotes an already registered local domain.");
            }
            domain = file.getName().substring(file.getName().indexOf('@')+1);
            userDirs = domainDirs.get(domain);
            if (userDirs==null) {
               userDirs = new LinkedList<String>();
               domainDirs.put(domain, userDirs);
            }
            userDirs.add(file.getName().substring(0, file.getName().indexOf('@')));
         }
      }
      if (domainDirs.isEmpty()) {
         return true;
      }
      
      //Need to check for domain names that have the same lower case value
      Iterator<String> iterator;
      iterator = domainDirs.keySet().iterator();
      String domainName, uniqueDomainName;
      Locale englishLocale = Locale.ENGLISH;
      Set<String> tempDirs = new LinkedHashSet<String>();
      Iterator<String> iter;
      String currentDomainName;
      while(iterator.hasNext()) {
         domainName = iterator.next();
         uniqueDomainName = domainName.toLowerCase(englishLocale);

         iter = tempDirs.iterator();
         while(iter.hasNext()) {
            currentDomainName = iter.next();
            if (!currentDomainName.equals(domainName)&&
                  currentDomainName.toLowerCase(englishLocale).equals(uniqueDomainName)) {
               throw new RuntimeException("Can not switch to new FileIO mode. There exist at "
                     + "least two registered domains("+domainName+","+currentDomainName+"), whose lower "
                     + "cased value is equal.");
            }
         }
         tempDirs.add(domainName);
      }
      tempDirs.clear();
      
      Map<File, List<String>> completed = new LinkedHashMap<File, List<String>>();
      String partialDomain = null;
      List<String> partialUsers = null;
      String partialUser = null;
      List<String> completedUsers;
      //Move the files
      boolean failed = false;
      File domainDir, oldUserDir;
      for(String aDomainDir:domainDirs.keySet()) {
         domainDir = new File(usersDir, aDomainDir);
         if (!domainDir.exists()) {
            if (!domainDir.mkdir()) {
               failed = true;
               break;
            }
         }
         completedUsers = new ArrayList<String>();
         for (String user:domainDirs.get(aDomainDir)) {
            File newUserDir = new File(domainDir, user);
            if (!newUserDir.exists()) {
               if (!newUserDir.mkdir()) {
                  partialDomain = aDomainDir;
                  partialUsers = completedUsers;
                  failed = true;
                  break;
               }
            }
            oldUserDir = new File(usersDir, user+'@'+aDomainDir);
            File[] userFiles = oldUserDir.listFiles();
            for (File aUserFile:userFiles) {
               if (!aUserFile.canRead()) {
                  partialDomain = aDomainDir;
                  partialUsers = completedUsers;
                  failed = true;
                  break;
               }
               if (!aUserFile.renameTo(new File(newUserDir, aUserFile.getName()))) {
                  log.warn("Move operation failed for file: "+aUserFile);
                  partialDomain = aDomainDir;
                  partialUsers = completedUsers;
                  partialUser = aUserFile.getName();
                  failed = true;
                  break;
               }
            }
            completedUsers.add(user);
         }
         completed.put(domainDir, completedUsers);
      }
      try {
         new File(usersDir, ".switched").createNewFile();
      }
      catch (IOException ioe) {
         failed = true;
      }
      //Check that the operation has successfully concluded
      if (failed) {
         log.warn("Can not switch to new FileIO mode. Could not successfully transfer all the "
               + "user messages to their enclosing domain folder. Reverting.");
         if (partialUser!=null) {
            File[] userFiles = new File(new File(usersDir, partialDomain), partialUser).listFiles();
            for (File aUserFile:userFiles) {
               if (!aUserFile.renameTo(new File(new File(usersDir, partialUser+'@'+partialDomain), aUserFile.getName()))) {
                  log.error("Renaming file "+aUserFile.getName()+" under "+partialDomain+" failed.");
               }
            }
         }
         if (partialDomain!=null) {
            for (String aPartialUser:partialUsers) {
               File[] userFiles = new File(new File(usersDir, partialDomain), aPartialUser).listFiles();
               for (File aUserFile:userFiles) {
                  if (!aUserFile.renameTo(new File(new File(usersDir, partialUser+'@'+partialDomain), aUserFile.getName()))) {
                     log.error("Renaming file "+aUserFile.getName()+" under "+partialDomain+" failed.");
                  }
               }
            }
         }
         File aFolder;
         boolean dirGone;
         for (File completedDomain:completed.keySet()) {
            for (String completedUser:completed.get(completedDomain)) {
               File[] userFiles = new File(new File(usersDir, completedDomain.getName()), completedUser).listFiles();
               for (File aUserFile:userFiles) {
                  if (!aUserFile.renameTo(new File(new File(usersDir, completedUser+'@'+completedDomain.getName()), aUserFile.getName()))) {
                     log.error("Renaming file "+aUserFile.getName()+" under "+completedUser+" failed.");
                  }
               }
               aFolder = new File(new File(usersDir, completedDomain.getName()), completedUser);
               dirGone = aFolder.delete();
               if (!dirGone) {
                  if (log.isDebugEnabled()) {
                     log.debug("Folder "+aFolder.getName()+" under "+completedDomain.getName()+" not deleted. Ignoring...");
                  }
               }
            }
            aFolder = new File(usersDir, completedDomain.getName());
            dirGone = aFolder.delete();
            if (!dirGone) {
               if (log.isDebugEnabled()) {
                  log.debug("Folder "+aFolder.getName()+" under "+usersDir.getName()+" not deleted. Ignoring...");
               }
            }
         }
         
         log.warn("Successfully reverted the user repository to its previous state.");
         return false;
      }
      else {
         File aFolder;
         boolean dirGone;
         for (File completedDomain:completed.keySet()) {
            for (String completedUser:completed.get(completedDomain)) {
               aFolder = new File(usersDir, completedUser+'@'+completedDomain.getName());
               dirGone = aFolder.delete();
               if (!dirGone) {
                  if (log.isDebugEnabled()) {
                     log.debug("Folder "+aFolder.getName()+" under folder users not deleted. Ignoring...");
                  }
               }
            }
         }
         log.warn("Successfully switched to the new FileIO mode.");
         return true;
      }
   }

   public void createDirectories() throws UserCreationException {
      
      if (dirRequests.isEmpty()) {
         return;
      }
      if (log.isDebugEnabled()) {
         log.debug("Creating Directories");
      }
      if (dirRequests.contains("users")) {
         dirRequests.remove("users");
         File usersDir = new File(getUsersDirectory());
         if (!usersDir.exists()) {
            log.info("Users directory does not exist.  Creating: " + usersDir.getAbsolutePath());
            if (!usersDir.mkdir()) {
               log.error("Error creating users directory: " + usersDir.getAbsolutePath() + ".");
               throw new RuntimeException("Unable to create users Directory.");
            }
         }
         try {
            new File(usersDir, ".switched").createNewFile();
         }
         catch (IOException ioe) {
            log.error("Error while marking the users directory as switched.");
            throw new RuntimeException("Error while marking the users directory as switched.");
         }
      }
      
      boolean switched = cm.isLegacyFileIOMode()||
            new File(getUsersDirectory(), ".switched").exists();
      if(!switched) {
         switched = checkLegacyFileIO(getUsersDirectory(), cm.getBackEnd().getDomains());
      }
      
      Iterator<String> iter = dirRequests.iterator();
      String directory;
      LocalDeliveryProcessor ldp = LocalDeliveryFactory.getInstance().getLocalDeliveryProccessor();
      List<String> users = new ArrayList<String>();
      while (iter.hasNext()) {
         directory = iter.next();
         if (directory.equals("smtp")) {
            File smtpDir = new File(getSMTPDirectory());
            if (smtpDir.exists()) {
               continue;
            }
            log.info("SMTP Mail directory does not exist.  Creating: " + smtpDir.getAbsolutePath());
            if (!smtpDir.mkdir()) {
               log.error("Error creating SMTP Mail directory: " + smtpDir.getAbsolutePath() + ". No incoming mail will be accepted!");
               throw new RuntimeException("Unable to create SMTP Mail Directory.");
            }
         } else if (directory.equals("amavis")) {
            File amavisDir = new File(cm.getAmavisSMTPDirectory());
            if (amavisDir.exists()) {
               continue;
            }
            log.info("Amavis SMTP Mail directory does not exist.  Creating: " + amavisDir.getAbsolutePath());
            if (!amavisDir.mkdir()) {
               log.error("Error creating Amavis SMTP Mail directory: " + amavisDir.getAbsolutePath() + ". No incoming mail will be accepted!");
               throw new RuntimeException("Unable to create Amavis SMTP Mail Directory.");
            }
         } else if (directory.equals("backup")) {
            File backup = new File(getBackupDirectory());
            if (!backup.mkdir()) {
               log.error("Error creating backup directory: " + backup.getAbsolutePath() + ".");
               throw new RuntimeException("Unable to create backup Directory.");
            }
         } else if (directory.equals("failed")) {
            File failedDir = new File(getFailedDirectory());
            log.info("failed directory does not exist.  Creating: " + failedDir.getAbsolutePath());
            if (!failedDir.mkdir()) {
               log.error("Error creating failed directory: " + failedDir.getAbsolutePath() + ".  No failed mail will be stored!");
               throw new RuntimeException("Unable to create failed Directory.");
            }
         } else if (directory.equals("external")) {
            File externalDir = new File(getRootDirectory(), "external");
            if (!externalDir.mkdir()) {
               log.error("Error creating external directory: " + externalDir.getAbsolutePath() + ".");
               throw new RuntimeException("Unable to create external Directory.");
            }
         } else if (directory.startsWith("testing:")) {
            String dirToCreate = directory.substring(directory.indexOf(':') + 1);
            File testingDirectory = new File(dirToCreate);
            if (!testingDirectory.exists()) {
               if (!testingDirectory.mkdir()) {
                  log.error("Error creating testing directory: " + testingDirectory.getAbsolutePath() + ".");
                  throw new RuntimeException("Unable to create testing Directory.");
               }
            }
         } else if (directory.length()>8&&directory.substring(0,8).equals(".domain@")) {
            String dirToCreate = directory.substring(directory.indexOf('@') + 1);
            File domainDirectory = new File(cm.getUsersDirectory(), dirToCreate);
            if (!domainDirectory.exists()) {
               if (!domainDirectory.mkdir()) {
                  log.error("Error creating domain directory: " + domainDirectory.getAbsolutePath() + ".");
                  throw new RuntimeException("Unable to create domain "+dirToCreate+" Directory.");
               }
               else {
                  log.info("Created domain directory: "+dirToCreate);
               }
            }
         }
         else {
            users.add(directory);
         }
      }
      if (switched) {
         iter = users.iterator();
         while(iter.hasNext()) {
            ldp.createUserRepository(iter.next());
         }
      }
      dirRequests.clear();
   }
   
   /** The root directory of the application. */
   private String rootDirectory;
   /** A directory used for doBackup purposes. */
   private String backupDirectory;
   /** The root directory used to store the server certificate and private key. **/
   private String securityDirectory;

   /**
    * The root directory used to store the incoming and outgoing messages.
    *
    * @return String
    */
   public String getRootDirectory() {
      return rootDirectory;
   }

   /**
    * The root directory used to store the incoming and outgoing messages.
    *
    * @param rootDirectory String
    */
   private void setRootDirectory(String rootDirectory) {
      this.rootDirectory = rootDirectory;
   }

   /**
    * The directory used to store doBackup files.
    *
    * @return backupDirectory String
    */
   public String getBackupDirectory() {
      return backupDirectory;
   }

   /**
    * The directory used to store doBackup files.
    *
    * @param backupDirectory String
    */
   private void setBackupDirectory(String backupDirectory) {
      this.backupDirectory = backupDirectory;
      File backup = new File(backupDirectory);
      if (!backup.exists()) {
         requestDirCreation("backup");
      }
   }

   /**
    * The directory used to hold security sensitive data.
    * 
    * @return securityDirectory String
    */
   public String getSecurityDirectory() {
      return securityDirectory;
   }

   /**
    * The root directory used to store the server certificate and private key.
    *
    * @param securityDirectory String
    */
   private void setSecurityDirectory(String securityDirectory) {
      this.securityDirectory = securityDirectory;
   }
   /** The directory used to store the incoming e-mails. */
   private String smtpDirectory;
   /** The directory used to store the user accounts. */
   private String usersDirectory;
   /** The directory used to store the failed messages. */
   private String failedDirectory;
   /** A directory to direct all messages to when in testing mode */
   private String testingDirectory;

   /**
    * The directory used to store incoming e-mails.
    * 
    * @return SMTPDirectory String
    */
   public String getSMTPDirectory() {
      return smtpDirectory;
   }

   /**
    * The directory used to store incoming e-mails.
    *
    * @param smtpDirectory String
    */
   private void setSMTPDirectory(String smtpDirectory) {
      this.smtpDirectory = smtpDirectory;
      File smtpDir = new File(smtpDirectory);
      if (!smtpDir.exists()) {
         requestDirCreation("smtp");
         return;
      }
      //If any incomplete temporary messages are present at application initialiazation delete them.
      File[] files = new File(smtpDirectory).listFiles(new FileFilter() {

         public boolean accept(File file) {
            if (file.getName().toLowerCase().endsWith(".tmp")) {
               return true;
            }
            return false;
         }
      });
      int numFiles = files.length;
      boolean fileGone;
      for (int i = 0; i < numFiles; i++) {
         fileGone = files[i].delete();
         if (!fileGone) {
            if (log.isDebugEnabled()) {
               log.debug("File "+files[i].getName()+" under "+smtpDirectory+" not deleted. Ignoring...");
            }
         }
      }
   }

   /**
    * The directory used to store the user accounts.
    *
    * @return String
    */
   public String getUsersDirectory() {
      return usersDirectory;
   }

   /**
    * The directory used to store the user accounts.
    *
    * @param usersDirectory String
    */
   private void setUsersDirectory(String usersDirectory) {
      this.usersDirectory = usersDirectory;
      File usersDir = new File(usersDirectory);
      // If the directory does not exist, create it.
      if (!usersDir.exists()) {
         requestDirCreation("users");
      }
   }

   /**
    * The directory used to store failed e-mails.
    *
    * @return failedDirectory String
    */
   public String getFailedDirectory() {
      return failedDirectory;
   }

   /**
    * The directory used to store failed e-mails.
    *
    * @param failedDirectory String
    */
   private void setFailedDirectory(String failedDirectory) {
      this.failedDirectory = failedDirectory;
      File failedDir = new File(failedDirectory);
      // If the directory does not exist, create it.
      if (!failedDir.exists()) {
         requestDirCreation("failed");
      }
   }

   public String getTestingDirectory() {
      return testingDirectory;
   }

   void directoriesConfiguration(Element element) {

      if (!cm.isFixed()) {
         String smtpDirectory = ((Element) element.getElementsByTagName("SMTP").item(0)).getTextContent();
         if (smtpDirectory==null||smtpDirectory.trim().length()==0) {
            smtpDirectory = getRootDirectory()+File.separator+"smtp";
         }
         else {
            smtpDirectory = smtpDirectory.trim();
         }
         setSMTPDirectory(smtpDirectory);
         log.info("JES incoming SMTP spool " + getSMTPDirectory());
         System.setProperty("jes.incoming.directory", getSMTPDirectory());

         String usersDirectory = ((Element) element.getElementsByTagName("users").item(0)).getTextContent();
         if (usersDirectory==null||usersDirectory.trim().length()==0) {
            usersDirectory = getRootDirectory()+File.separator+"users";
         }
         else {
            usersDirectory = usersDirectory.trim();
         }
         setUsersDirectory(usersDirectory);
         System.setProperty("jes.users.directory", getUsersDirectory());

         String failedDirectory = ((Element) element.getElementsByTagName("failed").item(0)).getTextContent();
         if (failedDirectory==null||failedDirectory.trim().length()==0) {
            failedDirectory = getRootDirectory()+File.separator+"failed";
         }
         else {
            failedDirectory = failedDirectory.trim();
         }
         setFailedDirectory(failedDirectory);
         System.setProperty("jes.failed.directory", getFailedDirectory());

         testingDirectory = ((Element) element.getElementsByTagName("testing").item(0)).getTextContent();
         if (testingDirectory!=null&&testingDirectory.trim().length()!=0) {
            testingDirectory = testingDirectory.trim();
            if (testingDirectory.equals(smtpDirectory)) {
               throw new RuntimeException("The testing directory can not be the same as the SMTP directory");
            }
            cm.testingMode();
            requestDirCreation("testing:" + testingDirectory);
            System.setProperty("jes.testing.directory", getTestingDirectory());
         } else {
            System.setProperty("jes.testing.directory", getSMTPDirectory());
         }
      }
   }
}
