package org.jblooming.ontology;

import org.jblooming.waf.settings.ApplicationState;

public interface Hidrator<V> {

  public V hidrate(String succ);

}
