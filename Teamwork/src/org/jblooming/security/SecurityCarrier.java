package org.jblooming.security;

import org.jblooming.agenda.ScheduleSupport;

import java.util.Iterator;

public interface SecurityCarrier {
  boolean isEnabled();

  void setEnabled(boolean enabled);

  boolean isAdministrator();

  void setAdministrator(boolean administrator);

  Iterator<Role> getInheritedRoleIterator();

  int rolesSize();

  ScheduleSupport getEnabledOnlyOn();

  void setEnabledOnlyOn(ScheduleSupport enabledOnlyOn);
}
