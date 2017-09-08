/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.basicimpl;

import net.wimpi.pim.contact.model.Contact;
import net.wimpi.pim.contact.model.Organization;
import net.wimpi.pim.contact.model.OrganizationalIdentity;

/**
 * A basic and simple implementation of an
 * {@link net.wimpi.pim.contact.model.OrganizationalIdentity}.
 * <p>
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 */
public class OrganizationalIdentityImpl
    implements OrganizationalIdentity {

  static final long serialVersionUID = 8690148566948064186L;

  //instance attributes
  protected String m_Title;
  protected String m_Role;
  //instance associations
  transient protected Contact m_Agent;
  protected Organization m_Organization;


  public OrganizationalIdentityImpl() {
  }//constructor

  public String getTitle() {
    return m_Title;
  }//getTitle

  public void setTitle(String title) {
    m_Title = title;
  }//setTitle

  public String getRole() {
    return m_Role;
  }//getRole

  public void setRole(String role) {
    m_Role = role;
  }//setRole

  public Contact getAgent() {
    return m_Agent;
  }//getAgent

  public void setAgent(Contact agent) {
    m_Agent = agent;
  }//setAgent

  public Organization getOrganization() {
    return m_Organization;
  }//getOrganization

  public void setOrganization(Organization org) {
    m_Organization = org;
  }//setOrganization

  public boolean hasOrganization() {
    return (m_Organization != null);
  }//hasOrganization

}//class OrganizationalIdentityImpl
