package org.jblooming.ontology;

import java.util.Collection;
import java.util.List;


public interface Node extends Identifiable {

  public Node getParentNode();

  public Collection getChildrenNode();

  public void setParentNode(Node node);

  public List<Node> getAncestors();


}

