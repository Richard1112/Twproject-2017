package org.jblooming.operator;

import org.hibernate.HibernateException;
import org.jblooming.ApplicationException;
import org.jblooming.agenda.ScheduleSupport;
import org.jblooming.ontology.HideableIdentifiableSupport;
import org.jblooming.persistence.hibernate.HibernateFactory;
import org.jblooming.security.*;
import org.jblooming.security.SecurityException;
import org.jblooming.utilities.JSP;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.waf.constants.OperatorConstants;
import org.jblooming.waf.constants.SecurityConstants;

import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */

public abstract class User extends HideableIdentifiableSupport implements Securable, SecurityCarrier, PermissionCache.PermissionCacheEnabled {

  private boolean administrator;
  private String loginName;
  private String password;
  /**
   * This can be used to match an external authentication server
   */
  private String authentication;

  private List lastPasswordList = new ArrayList();
  private Date lastPasswordChangeDate;
  private String personalIdentificationQuestion;
  private String personalIdentificationAnswer;
  private String name;
  private String surname;
  private boolean enabled = true;
  private ScheduleSupport enabledOnlyOn;

  // read only colls
  private Set<OperatorRole> operatorRoles = new HashSet<OperatorRole>();
  private Set<OperatorGroup> operatorGroups = new HashSet<OperatorGroup>();

  // flattened collections
  private Set<Role> inheritedRoles;
  private Set<Group> inheritedGroups;

  private Set<OperatorGroup> getOperatorGroups() {
    return operatorGroups;
  }

  public Iterator<OperatorGroup> getOperatorGroupsIterator() {
    //to avoid concurrent modifications in case of removal
    return new HashSet<OperatorGroup>(operatorGroups).iterator();
  }

  public int operatorGroupsSize() {
    return operatorGroups.size();
  }

  public Iterator<OperatorRole> getOperatorRolesIterator() {
    //to avoid concurrent modifications in case of removal
    return new HashSet<OperatorRole>(getOperatorRoles()).iterator();
  }

  public int operatorRolesSize() {
    return getOperatorRoles().size();
  }

  public boolean directRolesContain(Role role) {
    boolean result = false;
    for (OperatorRole or : getOperatorRoles()) {
      if (or.getRole().equals(role)) {
        result = true;
        break;
      }
    }
    return result;
  }

  public boolean directGroupsContain(Group group) {
    boolean result = false;
    for (OperatorGroup or : operatorGroups) {
      if (or.getGroup().equals(group)) {
        result = true;
        break;
      }
    }
    return result;
  }

  /**
   * Method setSurname
   *
   * @param surname a  String
   */
  public void setSurname(String surname) {
    this.surname = surname;
  }

  /**
   * Method getSurname
   *
   * @return a String
   */
  public String getSurname() {
    return surname;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * @deprecated use hasPermissionAsAdmin
   */
  public boolean isAdministrator() {
    return administrator;
  }

  public void setAdministrator(boolean administrator) {
    this.administrator = administrator;
  }

  public String getLoginName() {
    return loginName;
  }

  public void setLoginName(String loginName) {
    this.loginName = loginName;
  }

  public String getPassword() {
    return password;
  }

  public void setPassword(String password) {
    this.password = password;
  }

  public List getLastPasswordList() {
    return lastPasswordList;
  }

  public void setLastPasswordList(List lastPasswordList) {
    this.lastPasswordList = lastPasswordList;
  }

  public void addLastPassword(String password) {
    lastPasswordList.add(password);
  }

  public void removeLastPasswordList(String password) {
    lastPasswordList.remove(password);
  }

  public Iterator getLastPasswordIterator() {
    return lastPasswordList.iterator();
  }

  public Date getLastPasswordChangeDate() {
    return lastPasswordChangeDate;
  }

  public void setLastPasswordChangeDate(Date lastPasswordChangeDate) {
    this.lastPasswordChangeDate = lastPasswordChangeDate;
  }

  public String getPersonalIdentificationQuestion() {
    return personalIdentificationQuestion;
  }

  public void setPersonalIdentificationQuestion(String personalIdentificationQuestion) {
    this.personalIdentificationQuestion = personalIdentificationQuestion;
  }

  public String getPersonalIdentificationAnswer() {
    return personalIdentificationAnswer;
  }

  public void setPersonalIdentificationAnswer(String personalIdentificationAnswer) {
    this.personalIdentificationAnswer = personalIdentificationAnswer;
  }

  /**
   * Minimal implementation is u.hasPermissionFor(p), but should be refined in function of application business logic
   */
  public boolean hasPermissionFor(User u, Permission p) {
    return PermissionCache.hasPermissionFor(u,this,p);
  }
  public boolean hasPermissionForUnCached(User u, Permission p) {
    return u.hasPermissionFor(p);
  }

  public boolean hasPermissionAsAdmin() {

    if (isAdministrator())
      return true;

    if (getId().equals(OperatorConstants.SYSTEM_OPERATOR_ID))
      return true;

    //do you belong to a group of admins ?
    for (Group group : getInheritedGroups()) {
      if (group.isAdministrator())
        return true;
    }
    return false;
  }


  public boolean hasPermissionFor(Permission p) {
    return PermissionCache.hasPermissionFor(this,p);
  }

  public boolean hasPermissionForUnCached(Permission p) {

    if (hasPermissionAsAdmin())
      return true;

    for (Role role : getInheritedRoles()) {
      if (role.hasPermissionFor(p))
        return true;
    }

    return false;
  }

  public void testPermission(Permission permission) throws SecurityException {
    SecurityException.riseExceptionIfNoPermission(hasPermissionFor(permission), permission, this);
  }

  public void testIsAdministrator() throws SecurityException {
    if (!hasPermissionAsAdmin())
      throw new SecurityException(SecurityConstants.I18N_PERMISSION_LACKING);
  }

  public void changePassword(String password) throws ApplicationException {
    try {
      String prefixedPassword = computePassword(password);
      if (this.getPassword() == null || !this.getPassword().equals(prefixedPassword))
        setLastPasswordChangeDate(new Date());
      this.setPassword(prefixedPassword);
      getLastPasswordList().add(prefixedPassword);

    } catch (NoSuchAlgorithmException e) {
      throw new ApplicationException(e);
    }
  }

  public String getFullname() {
    String dn = JSP.w(getName());
    dn = dn + (JSP.ex(dn) && JSP.ex(getSurname()) ? " " : "") + JSP.w(getSurname());
    dn = JSP.ex(dn) ? dn : JSP.w(getLoginName());
    return dn;
  }

  public String getDisplayName() {
    return getFullname();
  }

  public Iterator<Role> getInheritedRoleIterator() {
    return getInheritedRoles().iterator();
  }

  public int rolesSize() {
    return getRoles().size();
  }

  public int compare(Object o1, Object o2) {
    if (o1 == null || o2 == null || ((Operator) o1).getSurname() == null || ((Operator) o2).getSurname() == null)
      return 0;
    return ((Operator) o1).getSurname().compareToIgnoreCase(((Operator) o2).getSurname());
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }


  /**
   * @return includes directly owned roles
   */
  public Set<Role> getInheritedRoles() {
    if (inheritedRoles == null) {
      inheritedRoles = new HashSet();
    }
    inheritedRoles.addAll(getRoles());
    for (Group group : getInheritedGroups()) {
      inheritedRoles.addAll(group.getInheritedRoles());
    }

    return inheritedRoles;
  }


  private Collection<Role> getRoles() {
    Set<Role> roles = new HashSet();
    for (OperatorRole operatorRole : getOperatorRoles()) {
      roles.add(operatorRole.getRole());
    }
    return roles;
  }

  public Set<Group> getInheritedGroups() {
    if (inheritedGroups == null) {
      inheritedGroups = new HashSet();
    }
    for (OperatorGroup operatorGroup : operatorGroups) {
      Group group = operatorGroup.getGroup();

      if (!inheritedGroups.contains(group)) {
        inheritedGroups.add(group);
        inheritedGroups.addAll(group.getInheritedGroups());
      }
    }
    return inheritedGroups;
  }


  /**
   * @return a schedule that specify when an operator is enabled to login. If null is alwais enabled
   */
  public ScheduleSupport getEnabledOnlyOn() {
    return enabledOnlyOn;
  }

  /**
   * @param enabledOnlyOn specify when an operator is enabled to login. If null is alwais enabled
   */
  public void setEnabledOnlyOn(ScheduleSupport enabledOnlyOn) {
    this.enabledOnlyOn = enabledOnlyOn;
  }

  public void userInitializer() throws HibernateException {
    HibernateFactory.initialize(operatorRoles);
    HibernateFactory.initialize(operatorGroups);
    HibernateFactory.initialize(inheritedRoles);
    HibernateFactory.initialize(inheritedGroups);
    getInheritedRoles();
    getInheritedGroups();
  }


  public String getAuthentication() {
    return authentication;
  }

  public void setAuthentication(String authentication) {
    this.authentication = authentication;
  }

  public Set<OperatorRole> getOperatorRoles() {
    return operatorRoles;
  }

  private void setOperatorRoles(Set<OperatorRole> operatorRoles) {
    this.operatorRoles = operatorRoles;
  }

  private void setOperatorGroups(Set<OperatorGroup> operatorGroups) {
    this.operatorGroups = operatorGroups;
  }

  public String computePassword(String password) throws NoSuchAlgorithmException {
    return StringUtilities.md5Encode(getLoginName() + password);
  }
}

