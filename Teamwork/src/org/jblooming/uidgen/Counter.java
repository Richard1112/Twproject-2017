package org.jblooming.uidgen;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.ontology.IdentifiableSupport;

import java.io.Serializable;

public class Counter extends IdentifiableSupport implements Serializable {

  protected String name;
  private int value;

  public Counter() {
  }

  public Counter(String name) {
    this.name = name;
    this.setValue(0);
  }

  public int getCounter() {
    return getValue();
  }

  public void setCounter(int counter) {
    setValue(counter);
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getNextValue() {
    int count = getValue() + 1;
    setValue(count);
    return String.valueOf(count);
  }

  public int getValue() {
    return value;
  }

  public void setValue(int value) {
    this.value = value;
  }

  public String getId() {
    return null;
  }

  public void setId(String id) {
    throw new PlatformRuntimeException("Cannot set id on Counter");
  }

}
