package org.jblooming.waf;

import org.jblooming.ontology.Identifiable;

import java.io.Serializable;

/**
 * @author Pietro Polsinelli ppolsinelli@open-lab.com
 * @author Roberto Bicchierai rbicchierai@open-lab.com
 */
public class ClipProxy {

  public Serializable id;
  public Class<? extends Identifiable> clazz;

  public ClipProxy(Serializable id, Class<? extends Identifiable> clazz) {
    this.id = id;
    this.clazz = clazz;
  }


}
