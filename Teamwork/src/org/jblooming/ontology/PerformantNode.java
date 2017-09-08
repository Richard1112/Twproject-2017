package org.jblooming.ontology;

import java.util.Iterator;
import java.util.Set;

/**
 * @author Pietro Polsinelli : ppolsinelli@open-lab.com
 */
public interface PerformantNode extends Node {

  static final String SEPARATOR = "^";

  String getAncestorIds();

  void setAncestorIds(String ancestorIds);

  int getDepth();

  Iterator<PerformantNodeSupport> getChildrenIterator();
  Set<PerformantNodeSupport> getChildren();

  int getChildrenSize();

}
