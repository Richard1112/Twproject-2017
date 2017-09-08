package org.jblooming.security;

import org.jblooming.anagraphicalData.AnagraphicalData;

public class LdapUser {

  String name;
  String surname;
  String username;
  String userPrincipalName;
  AnagraphicalData anag= new AnagraphicalData();
  boolean enabled;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getSurname() {
    return surname;
  }

  public void setSurname(String surname) {
    this.surname = surname;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getCity() {
    return anag.getCity();
  }

  public void setCity(String city) {
    anag.setCity(city);
  }
  public String getEmail() {
    return anag.getEmail();
  }

  public void setEmail(String email) {
    anag.setEmail(email);
  }

  public String getTelephone() {
    return anag.getTelephone();
  }

  public void setTelephone(String telephone) {
    anag.setTelephone(telephone);
  }


   public String getCountry() {
    return anag.getCountry();
  }

   public void setCountry(String country) {
    anag.setCountry(country);
  }

  public String getZip() {
    return anag.getZip();
  }

   public void setZip(String zip) {
    anag.setZip(zip);
  }

   public String getState() {
    return anag.getState();
  }

   public void setState(String state) {
    anag.setState(state);
  }
  
   public String getMobile() {
    return anag.getMobile();
  }

  public void setMobile(String mobile) {
    anag.setMobile(mobile);
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public String getUserPrincipalName() {
    return userPrincipalName;
  }

  public void setUserPrincipalName(String userPrincipalName) {
    this.userPrincipalName = userPrincipalName;
  }

  public String getDisplayUser() {
    if (name == null)
      name = "";
    if (surname == null)
      surname = "";
    if (username == null)
      username = "";
    return name + " " + surname + " (" + username + ")";
  }
}
