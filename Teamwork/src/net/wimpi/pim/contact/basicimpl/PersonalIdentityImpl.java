/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Image;
import net.wimpi.pim.contact.model.PersonalIdentity;
import net.wimpi.pim.util.StringUtil;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * A basic and simple implementation of a
 * {@link net.wimpi.pim.contact.model.PersonalIdentity}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class PersonalIdentityImpl
    implements PersonalIdentity {

  static final long serialVersionUID = -9017257204168808859L;

  //instance attributes
  protected String m_FormattedName;
  protected String m_Firstname;
  protected String m_Lastname;
  protected String m_SortString;
  protected List m_AdditionalNames;
  protected List m_Nicknames;
  protected List m_Prefixes;
  protected List m_Suffixes;
  protected Date m_BirthDate;
  protected Image m_Photo;

  public PersonalIdentityImpl() {
    m_AdditionalNames =
        Collections.synchronizedList(new ArrayList(5));
    m_Nicknames =
        Collections.synchronizedList(new ArrayList(5));
    m_Prefixes =
        Collections.synchronizedList(new ArrayList(3));
    m_Suffixes =
        Collections.synchronizedList(new ArrayList(3));
  }//constructor

  public String getFirstname() {
    return m_Firstname;
  }//getFirstname

  public void setFirstname(String name) {
    m_Firstname = name;
  }//setFirstname

  public String getLastname() {
    return m_Lastname;
  }//getLastname

  public void setLastname(String name) {
    m_Lastname = name;
  }//setLastname

  public String getAdditionalNamesList() {
    return StringUtil.joinList(listAdditionalNames());
  }//getAdditionalNamesList

  public void setAdditionalNamesList(String list) {
    String[] names = StringUtil.splitList(list);
    m_AdditionalNames.clear();
    for (int i = 0; i < names.length; i++) {
      addAdditionalName(names[i]);
    }
  }//setAdditionalNamesList

  public String[] listAdditionalNames() {
    String[] names = new String[m_AdditionalNames.size()];
    return (String[]) m_AdditionalNames.toArray(names);
  }//listAdditionalNames

  public String getAdditionalName(int num)
      throws IndexOutOfBoundsException {
    return (String) m_AdditionalNames.get(num);
  }//getAdditonalName

  public String setAdditionalName(int num, String name) {
    return (String) m_AdditionalNames.set(num, name);
  }//setAdditionalName

  public void addAdditionalName(String name) {
    m_AdditionalNames.add(name);
  }//addAdditionalName

  public String removeAdditionalName(int num)
      throws IndexOutOfBoundsException {
    return (String) m_AdditionalNames.remove(num);
  }//removeAdditionalName

  public void removeAllAdditionalNames() {
    m_AdditionalNames.clear();
  }//removeAllAdditionalNames

  public int getAdditionalNameCount() {
    return m_AdditionalNames.size();
  }//getAdditionalNameCount

  public String getNicknamesList() {
    return StringUtil.joinList(listNicknames());
  }//getNicknamesList

  public String setNickname(int num, String name) {
    return (String) m_Nicknames.set(num, name);
  }//setNickname

  public void setNicknamesList(String list) {
    String[] names = StringUtil.splitList(list);
    m_Nicknames.clear();
    for (int i = 0; i < names.length; i++) {
      addNickname(names[i]);
    }
  }//setNicknamesList

  public String[] listNicknames() {
    String[] names = new String[m_Nicknames.size()];
    return (String[]) m_Nicknames.toArray(names);
  }//listNicknames

  public String getNickname(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Nicknames.get(num);
  }//getAdditonalName

  public void addNickname(String name) {
    m_Nicknames.add(name);
  }//addNickname

  public String removeNickname(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Nicknames.remove(num);
  }//removeNickname

  public void removeAllNicknames() {
    m_Nicknames.clear();
  }//removeAllNicknames

  public int getNicknameCount() {
    return m_Nicknames.size();
  }//getNicknameCount

  public String getPrefixesList() {
    return StringUtil.joinList(listPrefixes());
  }//getPrefixesList

  public void setPrefixesList(String list) {
    String[] prefixes = StringUtil.splitList(list);
    m_Prefixes.clear();
    for (int i = 0; i < prefixes.length; i++) {
      addPrefix(prefixes[i]);
    }
  }//setPrefixesList

  public String[] listPrefixes() {
    String[] prefx = new String[m_Prefixes.size()];
    return (String[]) m_Prefixes.toArray(prefx);
  }//listPrefixes

  public String getPrefix(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Prefixes.get(num);
  }//getPrefix

  public String setPrefix(int num, String prefix) {
    return (String) m_Prefixes.set(num, prefix);
  }//setPrefix

  public void addPrefix(String prefix) {
    m_Prefixes.add(prefix);
  }//addPrefix

  public String removePrefix(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Prefixes.remove(num);
  }//removePrefix

  public void removeAllPrefixes() {
    m_Prefixes.clear();
  }//removeAllPrefixes

  public String getSuffixesList() {
    return StringUtil.joinList(listSuffixes());
  }//getSuffixesList

  public void setSuffixesList(String list) {
    String[] suffixes = StringUtil.splitList(list);
    m_Suffixes.clear();
    for (int i = 0; i < suffixes.length; i++) {
      addSuffix(suffixes[i]);
    }
  }//setSuffixesList

  public String[] listSuffixes() {
    String[] suffx = new String[m_Suffixes.size()];
    return (String[]) m_Suffixes.toArray(suffx);
  }//listSuffixes

  public String getSuffix(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Suffixes.get(num);
  }//getSuffix

  public String setSuffix(int num, String suffix) {
    return (String) m_Suffixes.set(num, suffix);
  }//setSuffix

  public void addSuffix(String prefix) {
    m_Suffixes.add(prefix);
  }//addSuffix

  public String removeSuffix(int num)
      throws IndexOutOfBoundsException {
    return (String) m_Suffixes.remove(num);
  }//removeSuffix

  public void removeAllSuffixes() {
    m_Suffixes.clear();
  }//removeAllSuffixes

  public Date getBirthDate() {
    return m_BirthDate;
  }//getBirthDate

  public void setBirthDate(Date birthdate) {
    m_BirthDate = birthdate;
  }//setBirthDate

  public String getFormattedName() {
    return m_FormattedName;
  }//getFormattedName

  public void setFormattedName(String fn) {
    m_FormattedName = fn;
  }//setFormattedName

  public void setSortString(String sortstr) {
    m_SortString = sortstr;
  }//setSortString

  public String getSortString() {
    return m_SortString;
  }//getSortString

  public Image getPhoto() {
    return m_Photo;
  }//getPhoto

  public void setPhoto(Image photo) {
    m_Photo = photo;
  }//setPhoto

  public boolean hasPhoto() {
    return (m_Photo != null);
  }//hasPhoto

}//class PersonalIdentityImpl
