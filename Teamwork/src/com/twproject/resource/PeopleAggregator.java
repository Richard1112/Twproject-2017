package com.twproject.resource;

import java.util.Set;

public interface PeopleAggregator {

  public void setInherit(boolean inherit);

  public boolean isInherit();

  public void setPropagate(boolean propagate);

  public boolean isPropagate();

  public Set<Person> getPersons();

  public boolean isPersonIn(Person o);
}
