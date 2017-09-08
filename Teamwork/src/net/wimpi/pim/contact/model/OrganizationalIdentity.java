/***
 * jpim Java PIM Library
 * Copyright 2001-2003 jpim team.
 *
 * jpim is free software; you can distribute and use this source
 * under the terms of the BSD-style license received along with
 * the distribution.
 ***/
package net.wimpi.pim.contact.model;

import java.io.Serializable;


/**
 * An interface modeling the organizational
 * identity of a contact based on the types
 * and information defined by the vCard Mime
 * directory profile standard.
 * <p>
 * For reference see RFC 2426:<br>
 * 3.5 Organizational Types<br>
 * 3.5.1 TITLE Type Definition<br>
 * 3.5.2 ROLE Type Definition<br>
 * 3.5.4 AGENT Type Definition<br>
 * <br>
 * Note that the information concerning
 * the organization have been placed in
 * a seperate type (i.e. <tt>Organization</tt>.
 *
 * @author Dieter Wimberger
 * @version 0.1 (22/07/2003)
 *
 * @see net.wimpi.pim.contact.model.Organization
 */
public interface OrganizationalIdentity
    extends Serializable {

  /**
   * Returns the title.<br>
   * Title refers to job title, functional position or
   * function.
   *
   * @return the title as <tt>String</tt>.
   */
  public String getTitle();

  /**
   * Sets the organizational title.<br>
   * Title refers to job title, functional position or
   * function.
   *
   * @param title the title as <tt>String</tt>.
   */
  public void setTitle(String title);

  /**
   * Returns the organizational role.<br>
   * Role refers to the role, occupation,
   * or business category.
   *
   * @return the role as <tt>String</tt>.
   */
  public String getRole();

  /**
   * Sets the organizational role.<br>
   * Role refers to the role, occupation,
   * or business category.
   *
   * @param role the role as <tt>String</tt>.
   */
  public void setRole(String role);

  /**
   * Returns the agent.<br>
   * Agent refers to another person who will
   * act on behalf of the individual or organization.
   *
   * @return the agent as <tt>Contact</tt>.
   */
  public Contact getAgent();

  /**
   * Sets the agent.<br>
   * Agent refers to another person who will
   * act on behalf of the individual or organization.
   *
   * @param agent the agent as <tt>Contact</tt>.
   */
  public void setAgent(Contact agent);

  /**
   * Returns the <tt>Organization</tt> instance
   * with information about the organization.
   *
   * @return the organization as <tt>Organization</tt>.
   */
  public Organization getOrganization();

  /**
   * Sets the <tt>Organization</tt> instance
   * with information about the organization.
   *
   * @param org the organization as <tt>Organization</tt>.
   */
  public void setOrganization(Organization org);

  /**
   * Tests if this <tt>OrganizationalIdentity</tt> has an
   * <tt>Organization</tt> instance.
   *
   * @return true if it has, false otherwise.
   */
  public boolean hasOrganization();

}//interface OrganizationalIdentity
