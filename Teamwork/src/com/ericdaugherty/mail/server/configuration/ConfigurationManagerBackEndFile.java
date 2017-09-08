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
 * $Rev: 322 $
 * $Date: 2013-08-28 18:36:20 +0200 (Wed, 28 Aug 2013) $
 *
 ******************************************************************************/

package com.ericdaugherty.mail.server.configuration;

//Java imports

import java.io.*;
import java.security.GeneralSecurityException;
import java.util.*;
import java.util.Map.Entry;

import org.w3c.dom.Element;

//Log imports
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

//Local imports
import com.ericdaugherty.mail.server.Mail;
import com.ericdaugherty.mail.server.auth.Saslprep;
import com.ericdaugherty.mail.server.configuration.RunnableConfigCBC.AddUsersPLL1;

import static com.ericdaugherty.mail.server.configuration.Utils.*;

import com.ericdaugherty.mail.server.configuration.backEnd.FilePersistExecutor;
import com.ericdaugherty.mail.server.configuration.cbc.CBCExecutor;
import com.ericdaugherty.mail.server.configuration.cbc.NewUser;
import com.ericdaugherty.mail.server.errors.InvalidAddressException;
import com.ericdaugherty.mail.server.info.*;
import com.ericdaugherty.mail.server.utils.*;

/**
 * @author Andreas Kyrmegalos
 */
public final class ConfigurationManagerBackEndFile implements ConfigurationManagerBackEnd, ConfigurationParameterConstants {

  /**
   * Logger
   */
  //private static Log log = LogFactory.getLog(ConfigurationManagerBackEndFile.class);
  private static Log log = LogFactory.getLog("JESLogger");
  private final ConfigurationManager cm;
  private final Locale locale;
  /**
   * The file reference to the username.conf configuration file
   */
  private File userConfigurationFile;
  /**
   * The timestamp for the username.conf file when it was last loaded
   */
  private long userConfigurationFileTimestamp;
  /**
   * The file reference to the realms.conf configuration file
   */
  private File realmsConfigurationFile;
  /**
   * The file reference to the realms password file
   */
  private File realmsPasswordFile;
  private boolean realmsPassModified;
  private Properties userProperties;
  /**
   * A Map of Users keyed by their uniqueUsername
   */
  private Map<String, UserFile> users;
  /**
   * A Map of Realms keyed by their uniqueRealmName
   */
  private Map<String, Realm> realms;
  private Set<Realm> realmsForResponse;
  /**
   * A Map of the hex hash of (username:realm:pass) keyed by the
   * full realm name and the username.
   */
  private Map<String, Map<String, char[]>> realmsPass;
  /**
   * Array of domains with the default username that the SMTP server
   * should accept mail for local delivery
   */
  private Map<Domain, EmailAddress> localDomainsWithDefaultMailbox;
  private Domain defaultDomain;
  private boolean singleDomainMode;
  //A default domainName is not necessary specified (although that is the case with the File backend)

  ConfigurationManagerBackEndFile(ConfigurationManager cm) {

    this.cm = cm;
    this.locale = cm.englishLocale;
    String rootDirectory = cm.getRootDirectory();

    String userConfigFilename = "user.conf";
    String realmsConfigFilename = "realms.conf";

    // Verify the User config file exists.
    File userConfigFile = new File(rootDirectory + File.separator + "conf", userConfigFilename);
    if (!userConfigFile.isFile() || !userConfigFile.exists()) {
      throw new RuntimeException("Invalid user.conf ConfigurationFile! " + userConfigFile.getAbsolutePath());
    }

    // Verify the Realms config file exists.
    File realmsConfigFile = new File(rootDirectory + File.separator + "conf", realmsConfigFilename);
    if (!realmsConfigFile.isFile() || !realmsConfigFile.exists()) {
      throw new RuntimeException("Invalid realms.conf ConfigurationFile! " + userConfigFile.getAbsolutePath());
    }

    this.userConfigurationFile = userConfigFile;
    this.realmsConfigurationFile = realmsConfigFile;

  }

  public void init(Element element) {

    String domains = element.getElementsByTagName("domains").item(0).getTextContent();
    String defaultUserString = element.getElementsByTagName("defaultMailbox").item(0).getTextContent();
    if (domains == null) {
      throw new RuntimeException("No Local Domains defined! Can not run without local domains defined.");
    }

    updateDomains(domains, defaultUserString);
  }

  /**
   * Not implemented in a file scope *
   */
  public void shutdown() {
  }

  public void restore(String backupDirectory) throws IOException {

    FileUtils.copyFile(new File(backupDirectory, "user.bak"), userConfigurationFile);
    FileUtils.copyFile(new File(backupDirectory, "realms.bak"), realmsConfigurationFile);
    FileUtils.copyFile(new File(backupDirectory, "realmpwd.bak"), new File(cm.getSecurityDirectory(), "realms.dat"));
  }

  public void doBackup(String backupDirectory) throws IOException {

    try {
      FileUtils.copyFile(new File(backupDirectory, "user.bak"), new File(backupDirectory, "user.ba2"));
      FileUtils.copyFile(new File(backupDirectory, "realms.bak"), new File(backupDirectory, "realms.ba2"));
      FileUtils.copyFile(new File(backupDirectory, "realmpwd.bak"), new File(backupDirectory, "realmpwd.ba2"));
    } catch (IOException e) {
      if (log.isDebugEnabled()) {
        log.debug(e.getMessage());
      }
    }

    FileUtils.copyFile(userConfigurationFile, new File(backupDirectory, "user.bak"));
    FileUtils.copyFile(realmsConfigurationFile, new File(backupDirectory, "realms.bak"));
    FileUtils.copyFile(new File(cm.getSecurityDirectory(), "realms.dat"), new File(backupDirectory, "realmpwd.bak"));
  }

  public void doWeeklyBackup(String backupDirectory) throws IOException {

    FileUtils.copyFile(userConfigurationFile, new File(backupDirectory, "user.wek"));
    FileUtils.copyFile(realmsConfigurationFile, new File(backupDirectory, "realms.wek"));
    FileUtils.copyFile(new File(cm.getSecurityDirectory(), "realms.dat"), new File(backupDirectory, "realmpwd.wek"));
  }

  private Properties loadUsers() {

    DelimitedInputStream dis = null;
    try {
      dis = new DelimitedInputStream(new FileInputStream(userConfigurationFile), 2048, true);
      JESProperties jesProperties = new JESProperties(dis, true);
      jesProperties.load();
      return jesProperties.getProperties();
    } catch (IOException e) {
      // All checks should be done before we get here, so there better
      // not be any errors.  If so, throw a RuntimeException.
      log.error(e.getLocalizedMessage());
      throw new RuntimeException("Error Loading User Configuration File!  Unable to continue Operation.");
    } finally {
      if (null != dis) {
        try {
          dis.close();
        } catch (IOException ex) {
          log.debug(ex.getMessage());
        }
      }
    }
  }

  private Map<String, UserFile> loadUserProperties() {

    userProperties = loadUsers();

    Map<String, UserFile> users = new HashMap<String, UserFile>();
    Enumeration propertyKeys = userProperties.keys();
    String key;
    String fullUsername;
    Domain domain;
    int atPos;
    UserFile user;
    List<Domain> domains = new ArrayList<Domain>(localDomainsWithDefaultMailbox.keySet());
    while (propertyKeys.hasMoreElements()) {
      key = ((String) propertyKeys.nextElement()).trim();
      if (key.startsWith(USER_DEF_PREFIX)) {
        fullUsername = key.substring(USER_DEF_PREFIX.length());
        atPos = fullUsername.indexOf('@');
        if (atPos == -1) {
          log.error("Unable to load user: " + fullUsername + ". Skipping...");
          continue;
        }
        domain = new Domain(fullUsername.substring(atPos + 1));
        domain = domains.get(domains.indexOf(domain));
        try {
          user = loadUser(fullUsername, domain, userProperties);
          users.put(user.getUniqueName(), user);
        } catch (InvalidAddressException e) {
          log.error("Unable to load user: " + fullUsername + ". Skipping...");
        }
      }
    }
    if (users.isEmpty() && !(Mail.isTesting() || cm.isLocalTestingMode())) {
      log.warn("No users registered with any domain!!!");
    } else {
      if (log.isInfoEnabled()) {
        log.info("Loaded " + users.size() + " users from user.conf");
      }
    }
    return users;

  }

  private Map<String, Realm> loadRealmProperties() {

    Properties properties = null;
    DelimitedInputStream dis = null;
    try {
      dis = new DelimitedInputStream(new FileInputStream(realmsConfigurationFile), 2048);
      JESProperties jesProperties = new JESProperties(dis);
      jesProperties.load();
      properties = jesProperties.getProperties();
    } catch (IOException e) {
      // All checks should be done before we get here, so there better
      // not be any errors.  If so, throw a RuntimeException.
      throw new RuntimeException("Error Loading realms File!  Unable to continue Operation.");
    } finally {
      try {
        dis.close();
      } catch (IOException ex) {
        log.debug(ex.getMessage());
      }
    }

    Map<String, Realm> realms = new LinkedHashMap<String, Realm>();
    Iterator<Entry<Object, Object>> iter = properties.entrySet().iterator();
    Entry realmDefinition;
    String key;
    String collection;
    String fullRealmName;
    String uniqueRealmName;
    Realm realm;
    Domain domain;
    int atPos;
    List<Domain> domains = new ArrayList<Domain>(localDomainsWithDefaultMailbox.keySet());
    while (iter.hasNext()) {
      realmDefinition = iter.next();
      key = (String) realmDefinition.getKey();
      if (key.startsWith(REALM_DEF_PREFIX)) {
        fullRealmName = key.substring(REALM_DEF_PREFIX.length());
        atPos = fullRealmName.indexOf('@');
        if (atPos == -1) {
          log.warn("Will not add " + fullRealmName + " to the realm list. It is either a One-To-One" +
                  " map to a domain or miss-spelled");
          continue;
        }
        domain = new Domain(fullRealmName.substring(atPos + 1));
        if (domains.contains(domain)) {
          collection = fullRealmName.substring(0, atPos);
          //Retrieving the already defined domain to correct possible casing errors and reduce
          //memory usage
          domain = domains.get(domains.indexOf(domain));
          fullRealmName = collection + '@' + domain.getDomainName();
          uniqueRealmName = collection + '@' + domain.getUniqueName();
          if (realms.containsKey(uniqueRealmName)) {
            log.warn("Will not add " + uniqueRealmName + " to the realm list. The specified realm already exists.");
            continue;
          }
          realm = loadRealm(collection, domain, (String) realmDefinition.getValue());
          realms.put(realm.getUniqueName(), realm);
        } else {
          log.warn("The " + fullRealmName + " does not correspond to a local domain. Ignoring...");
        }
      }
    }
    domains.clear();

    if (log.isInfoEnabled()) {
      log.info("Loaded " + realms.size() + " realms from realms.conf");
    }

    realmsForResponse = null;
    return realms;

  }

  /**
   * A utility method to trim control characters and white-
   * space.
   *
   * @param input a character array
   * @return a array of characters free of control characters and
   *         whitespace
   */
  private char[] trimInput(char[] input) {

    if (input == null) return null;

    StringBuilder sb = new StringBuilder(input.length);
    char c;
    int start, finish = input.length;
    for (start = 0; start < input.length; start++) {
      c = input[start];
      if (!(Saslprep.contains(Saslprep.c21Entries, c) || c == ' ')) {
        break;
      }
    }
    if (start != input.length) {
      for (finish = input.length - 1; finish > start; finish--) {
        c = input[finish];
        if (!(Saslprep.contains(Saslprep.c21Entries, c) || c == ' ')) {
          finish++;
          break;
        }
      }
      if (start == finish) {
        throw new IndexOutOfBoundsException("The supplied password is invalid. Aborting...");
      }
    } else {
      start = 0;
    }

    sb.append(input, start, finish);
    char[] output = new char[sb.length()];
    sb.getChars(0, sb.length(), output, 0);
    return output;
  }

  /**
   * Creates a new User instance for the specified username
   * using the specified properties.
   *
   * @param fullAddress full username (me@mydomain.com) with preserved casing
   * @param properties  the properties that contain the username parameters.
   * @return a new User instance.
   */
  private UserFile loadUser(String fullAddress, Domain domain, Properties properties) throws InvalidAddressException {

    EmailAddress address = new EmailAddress(fullAddress.substring(0, fullAddress.indexOf('@')), domain);
    UserFile user = new UserFile(address);

    // Load the password
    char[] password = trimInput((char[]) properties.get(USER_DEF_PREFIX + fullAddress));

    JESVaultControl.getInstance().setUserPassword(user, password);

    // Load the 'forward' addresses.
    char[] forwardAddresses = (char[]) properties.get(USER_PROPERTY_PREFIX + fullAddress + USER_FILE_FORWARDS);
    String[] forwardAddressesList = {};
    if (forwardAddresses != null) {
      String forwardAddressesString = new String(forwardAddresses);
      if (forwardAddressesString.trim().length() >= 0) {
        forwardAddressesList = Utils.tokenize(forwardAddressesString, true, ",");
      }
    }
    List<EmailAddress> addressList = new ArrayList<EmailAddress>(forwardAddressesList.length);
    for (int index = 0; index < forwardAddressesList.length; index++) {
      try {
        addressList.add(new EmailAddress(forwardAddressesList[index]));
      } catch (InvalidAddressException e) {
        log.warn("Forward address: " + forwardAddressesList[index] + " for user " + user + " is invalid and will be ignored.");
      }
    }

    EmailAddress[] emailAddresses = new EmailAddress[addressList.size()];
    emailAddresses = (EmailAddress[]) addressList.toArray(emailAddresses);

    if (log.isDebugEnabled()) {
      log.debug(emailAddresses.length + " forward addresses load for user: " + user);
    }
    user.setForwardAddresses(emailAddresses);

    return user;
  }

  /**
   * Creates a new Realm instance for the specified Realm name
   * while adding the users associated with the realm
   *
   * @param collection the collection part of a realm
   * @param domain     the domain part of a a realm
   * @param userList   the String that contains the list of usernames.
   * @return a new Realm instance.
   */
  public Realm loadRealm(String collection, Domain domain, String userList) {

    Realm realm = new Realm(collection, domain);
    StringTokenizer st = new StringTokenizer(userList, ",");
    String username;
    User user;
    int atPos;
    while (st.hasMoreTokens()) {
      username = st.nextToken();
      atPos = username.indexOf('@');
      if (atPos == -1) {
        username += "@" + domain.getUniqueName();
      }
      //The constructed username corresponds to a uniqueUsername String.
      user = users.get(username);
      if (user != null) {
        realm.addUser(user);
      } else {
        log.warn("User " + username + " not listed in users. Skipping...");
      }
    }
    return realm;
  }

  public Map<String, Map<String, char[]>> encryptRealmPasswords() {

    realmsPassModified = false;
    Map<String, Map<String, char[]>> realmsPass = null;
    realmsPasswordFile = new File(cm.getSecurityDirectory(), "realms.dat");
    if (realmsPasswordFile.exists()) {
      ObjectInputStream ois = null;
      try {
        ois = new ObjectInputStream(new FileInputStream(realmsPasswordFile));
        realmsPass = (Map<String, Map<String, char[]>>) ois.readObject();
      } catch (Exception e) {
        throw new RuntimeException("Error loading realms password File!  Unable to continue Operation.");
      } finally {
        if (ois != null) {
          try {
            ois.close();
          } catch (IOException ex) {
            log.debug(ex.getMessage());
          }
          ois = null;
        }
      }
    } else {
      realmsPass = new LinkedHashMap<String, Map<String, char[]>>();
    }
    try {

      //Add/update new realms and/or new users

      //It is safe to assume that if the null realm in not present in the realms password file
      //that this is the first time JES runs and all username passwords are still plaintext.
      //We also add all the domains as realms here.
      if (!realmsPass.containsKey(Realm.getNullRealm().getUniqueName())) {
        Map<String, char[]> nullRealm = new HashMap<String, char[]>();
        Map<String, Map<String, char[]>> domainRealms = new LinkedHashMap<String, Map<String, char[]>>();
        for (Domain domain : localDomainsWithDefaultMailbox.keySet()) {
          domainRealms.put(domain.getUniqueName(), new HashMap<String, char[]>());
          cm.requestDirCreation(".domain@" + domain.getDomainName());
        }
        Iterator<UserFile> iter = users.values().iterator();
        UserFile user;
        Realm domainRealm;
        while (iter.hasNext()) {
          user = iter.next();
          //Request user dir creation here
          cm.requestDirCreation(new File(new File(cm.getUsersDirectory(), user.getDomain().getDomainName()), user.getUsername()).getPath());

          nullRealm.put(user.getUniqueName(), user.getHashedRealmPassword(Realm.getNullRealm()));
          domainRealm = new Realm(user.getDomain());
          domainRealms.get(domainRealm.getUniqueName()).put(user.getUniqueName(), user.getHashedRealmPassword(domainRealm));
        }
        realmsPassModified = true;
        realmsPass.put(Realm.getNullRealm().getUniqueName(), nullRealm);
        realmsPass.putAll(domainRealms);
      } else {
        Map<String, char[]> nullRealm = realmsPass.get(Realm.getNullRealm().getUniqueName());
        Map<String, Map<String, char[]>> domainRealms = new LinkedHashMap<String, Map<String, char[]>>();
        Map<String, Map<String, char[]>> newDomainRealms = new LinkedHashMap<String, Map<String, char[]>>();
        Iterator<Domain> domainIter = localDomainsWithDefaultMailbox.keySet().iterator();
        Domain domain;
        while (domainIter.hasNext()) {
          domain = domainIter.next();
          if (realmsPass.containsKey(domain.getUniqueName())) {
            domainRealms.put(domain.getUniqueName(), realmsPass.get(domain.getUniqueName()));
          } else {
            newDomainRealms.put(domain.getUniqueName(), new HashMap<String, char[]>());
            cm.requestDirCreation(".domain@" + domain.getDomainName());
          }
        }
        Iterator<UserFile> iter = users.values().iterator();
        Iterator<String> iterDomainRealm;
        Map<String, char[]> domainRealm;
        UserFile user;
        while (iter.hasNext()) {
          user = iter.next();
          domain = user.getDomain();
          if (!domainRealms.containsKey(domain.getUniqueName())) {
            newDomainRealms.get(domain.getUniqueName()).put(user.getUniqueName(), user.getHashedRealmPassword(new Realm(domain)));
            realmsPassModified = true;
          } else {
            iterDomainRealm = domainRealms.keySet().iterator();
            while (iterDomainRealm.hasNext()) {
              domainRealm = domainRealms.get(iterDomainRealm.next());
              if (!domainRealm.containsKey(user.getUniqueName()) || user.isClearTextPassword()) {
                domainRealm.put(user.getUniqueName(), user.getHashedRealmPassword(new Realm(domain)));
                realmsPassModified = true;
              }
            }
          }
          if (!nullRealm.containsKey(user.getUniqueName()) || user.isClearTextPassword()) {
            //Request user dir creation here
            cm.requestDirCreation(new File(new File(cm.getUsersDirectory(), user.getDomain().getDomainName()), user.getUsername()).getPath());

            nullRealm.put(user.getUniqueName(), user.getHashedRealmPassword(Realm.getNullRealm()));
            realmsPassModified = true;
          }
        }
        realmsPass.putAll(newDomainRealms);
      }

      String uniqueUsername;

      Iterator<String> iterOut = realms.keySet().iterator();
      String uniqueRealmName;
      UserFile user;
      while (iterOut.hasNext()) {
        uniqueRealmName = iterOut.next();
        if (!realmsPass.containsKey(uniqueRealmName)) {
          Map<String, char[]> singleRealmPass = new HashMap<String, char[]>();
          Iterator<User> iter = realms.get(uniqueRealmName).userIterator();
          while (iter.hasNext()) {
            user = users.get(iter.next().getUniqueName());
            if (user != null) {
              uniqueUsername = user.getUniqueName();
              if (user.isClearTextPassword()) {
                singleRealmPass.put(uniqueUsername, user.getHashedRealmPassword(realms.get(uniqueRealmName)));
              } else {
                log.error("User: " + user + " expected to have a password in clear text. Not "
                        + "able to compute the H1 for realm: " + realms.get(uniqueRealmName) + ". "
                        + "This is the result of adding an existing user (whose password has "
                        + "already been hashed) to a new Realm.");
              }
            }
          }
          realmsPassModified = true;
          realmsPass.put(uniqueRealmName, singleRealmPass);
        } else {
          Map<String, char[]> singleRealmPass = realmsPass.get(uniqueRealmName);
          Iterator<User> iter = realms.get(uniqueRealmName).userIterator();
          while (iter.hasNext()) {
            user = users.get(iter.next().getUniqueName());
            if (user != null) {
              uniqueUsername = user.getUniqueName();
              if (user.isClearTextPassword()) {
                singleRealmPass.put(uniqueUsername, user.getHashedRealmPassword(realms.get(uniqueRealmName)));
                realmsPassModified = true;
              } else if (!singleRealmPass.containsKey(uniqueUsername)) {
                log.error("User: " + user + " expected to have a password in clear text. Not "
                        + "able to compute the H1 for realm: " + realms.get(uniqueRealmName) + ". "
                        + "This is the result of adding an existing user (whose password has "
                        + "already been hashed) to an existing Realm.");
              }
            }
          }
          realmsPass.put(uniqueRealmName, singleRealmPass);
        }
      }

      //Remove deleted realms/users from the realms password file
      iterOut = realmsPass.keySet().iterator();
      Iterator<String> iterIn;
      Map<String, char[]> singleRealmPass;
      while (iterOut.hasNext()) {
        uniqueRealmName = iterOut.next();
        if (uniqueRealmName == null || uniqueRealmName.equals("null")) {
          continue;
        }
        if (!realms.containsKey(uniqueRealmName) && !isLocalDomain(uniqueRealmName)) {
          iterOut.remove();
          realmsPassModified = true;
        } else {
          singleRealmPass = realmsPass.get(uniqueRealmName);
          iterIn = singleRealmPass.keySet().iterator();
          while (iterIn.hasNext()) {
            if (!users.containsKey(iterIn.next())) {
              iterIn.remove();
              realmsPassModified = true;
            }
          }
        }
      }
    } catch (GeneralSecurityException e) {
      throw new RuntimeException("Error updating Realms. Unable to continue Operation.");
    }
    return realmsPass;
  }

  public void persistUsersAndRealms() {

    if (realmsPassModified) {
      persistRealmsPassFile();
    }
    persistUserConfFile();
  }

  private void persistRealmsPassFile() {

    if (!realmsPasswordFile.exists()) {
      try {
        if (!realmsPasswordFile.createNewFile()) {
          throw new IOException();
        }
      } catch (Exception e) {
        throw new RuntimeException("Error creating realms password File!  Unable to continue Operation.");
      }
    }

    realmsPassModified = false;
    ObjectOutputStream oos = null;
    try {
      oos = new ObjectOutputStream(new FileOutputStream(realmsPasswordFile));
      oos.writeObject(realmsPass);
    } catch (Exception e) {
      log.error(e.getLocalizedMessage());
      throw new RuntimeException("Error storing realms password File!  Unable to continue Operation.");
    } finally {
      if (oos != null) {
        try {
          oos.close();
        } catch (IOException ex) {
          log.debug(ex.getMessage());
        }
        oos = null;
      }
    }

  }

  private void persistUserConfFile() {

    boolean userConfModified = false;
    UserFile user;
    Iterator<UserFile> iter = users.values().iterator();
    String username;
    char[] password;
    char[] toPersist;
    while (iter.hasNext()) {
      user = iter.next();
      if (user.isClearTextPassword()) {
        username = user.getUserAdress();
        password = user.getEncryptedPassword();
        if (password == null) {
          log.error("Error encrypting plaintext password from user.conf for user " + username);
          throw new RuntimeException("Error encrypting password for user: " + username);
        }
        toPersist = new char[password.length + 5];
        System.arraycopy(ENC_C, 0, toPersist, 0, 5);
        System.arraycopy(password, 0, toPersist, 5, password.length);
        userProperties.put(USER_DEF_PREFIX + username, toPersist);
        userConfModified = true;
      }
    }

    // Save the username configuration if changed.
    if (userConfModified) {
      try {
        JESProperties.store(userProperties, userConfigurationFile, USER_PROPERTIES_HEADER);
        log.info("Changes to user.conf persisted to disk.");
      } catch (IOException e) {
        log.error("Unable to store changes to user.conf!  Plain text passwords were not hashed!");
        throw new RuntimeException("Error storing changes to user.conf.");
      }

    }

    userProperties.clear();
    userProperties = null;
    // Update the 'last loaded' timestamp.
    userConfigurationFileTimestamp = userConfigurationFile.lastModified();

  }

  public void updateThroughConnection(CBCExecutor cbcExecutor) {

    log.info(cbcExecutor);

    List<NewUser> newUsers = ((AddUsersPLL1) cbcExecutor).getNewUsers();
    FilePersistExecutor fpe = new FilePersistExecutor(users, realms, new ArrayList<Domain>(localDomainsWithDefaultMailbox.keySet()));
    fpe.insertUser(newUsers);
  }

  public boolean persistUserUpdate() {

    if (userConfigurationFile.lastModified() != userConfigurationFileTimestamp) {
      log.info("User Configuration File Changed, reloading...");
      return true;
    }
    return false;
  }

  public char[] getRealmPassword(Realm realm, EmailAddress emailAddress) {

    User user = getUser(emailAddress);
    if (user == null) {
      log.warn("User " + emailAddress.getAddress() + " does not exist. Can not retrieve realm password.");
      return null;
    }
    Map<String, char[]> realmPass = realmsPass.get(realm.getUniqueName());
    if (realmPass == null) {
      log.error("Could not locate the password map for realm " + realm.getFullRealmName());
      return null;
    }
    return realmPass.get(user.getUniqueName()).clone();
  }

  public void loadUsersAndRealms() {

    users = loadUserProperties();
    if (users.isEmpty() && !(Mail.isTesting() || cm.isLocalTestingMode())) {
      log.error("No users registered, aborting startup (non è vero). Please consult the documentation.");
      //throw new RuntimeException("No users registered, aborting startup. Please consult the documentation.");
    }
    realms = loadRealmProperties();
    realmsPass = encryptRealmPasswords();
  }

  public void updateUsersAndRealmPasswords() {

    users = loadUserProperties();
    realmsPass = encryptRealmPasswords();
  }

  public Set<Domain> getDomains() {
    return new LinkedHashSet<Domain>(localDomainsWithDefaultMailbox.keySet());
  }

  public Set<Realm> getRealms() {

    if (realmsForResponse == null) {
      Set<Domain> domains = localDomainsWithDefaultMailbox.keySet();
      realmsForResponse = new LinkedHashSet<Realm>(domains.size() + realms.size(), 1);
      for (Domain domain : domains) {
        realmsForResponse.add(new Realm(domain));
      }
      realmsForResponse.addAll(realms.values());
      realmsForResponse.remove(Realm.getNullRealm());
    }
    return realmsForResponse;
  }

  private void updateDomains(String domains, String defaultMailboxes) {

    String[] result1 = domains.trim().split("\\s+");
    String[] result2 = defaultMailboxes.trim().split("\\s+");

    if (result1.length == 0) {
      log.warn("No local domains registered with JES.");
      defaultDomain = Domain.getNullDomain();
      localDomainsWithDefaultMailbox = new LinkedHashMap<Domain, EmailAddress>(0);
      return;
    }

    //Weed out any improper e-mail addresses
    List<EmailAddress> mailboxes = new ArrayList<EmailAddress>(result2.length);
    for (String mailbox : result2) {
      try {
        //TODO retaining compatibility with previous versions by applying lower-case
        mailboxes.add(new EmailAddress(mailbox.toLowerCase(locale)));
      } catch (InvalidAddressException iae) {
        log.warn("E-mail address: " + mailbox + " is malformed and therefore ignored.");
      }
    }

    localDomainsWithDefaultMailbox = new LinkedHashMap<Domain, EmailAddress>(result1.length, 1);

    Domain domain;
    EmailAddress mailbox;
    for (String domainString : result1) {
      //TODO retaining compatibility with previous versions by applying lower-case
      try {
        domainString = EmailAddress.parseDomain(domainString.toLowerCase(locale));
      } catch (InvalidAddressException iae) {
        log.warn("Error parsing E-mail address: " + domainString);
        log.warn(iae.getMessage());
        continue;
      }
      if (domainString == null) {
        continue;
      }
      domain = new Domain(domainString);
      mailbox = null;
      for (int j = 0; j < mailboxes.size(); j++) {
        mailbox = mailboxes.get(j);
        if (mailbox.getDomain().equals(domain)) {
          break;
        } else {
          mailbox = null;
        }
      }
      localDomainsWithDefaultMailbox.put(domain, mailbox == null ? new EmailAddress() : mailbox);

    }
    try {
      defaultDomain = localDomainsWithDefaultMailbox.keySet().iterator().next();
    } catch (NoSuchElementException nsee) {
      defaultDomain = Domain.getNullDomain();
    }
    singleDomainMode = result1.length == 1;
  }

  public boolean isLocalDomain(String domain) {

    Domain _domain = new Domain(domain);
    return localDomainsWithDefaultMailbox.containsKey(_domain);
  }

  public boolean isSingleDomainMode() {
    return singleDomainMode;
  }

  public Domain getSingleDomain() {
    return singleDomainMode ? defaultDomain : null;
  }

  public Domain getDefaultDomain() {
    return defaultDomain;
  }

  public void updateDefaultDomain() {
  }

  public EmailAddress getDefaultMailbox(String domain) {
    Domain _domain = new Domain(domain);
    return localDomainsWithDefaultMailbox.get(_domain);
  }

  public UserFile getUser(EmailAddress address) {
    return users.get(address.getAddress().toLowerCase(cm.englishLocale));
  }

  public Realm getRealm(String realmName) {
    return realms.get(realmName);
  }

  public File getUserConfigurationFile() {
    return userConfigurationFile;
  }

  public File getRealmsConfigurationFile() {
    return realmsConfigurationFile;
  }
}
