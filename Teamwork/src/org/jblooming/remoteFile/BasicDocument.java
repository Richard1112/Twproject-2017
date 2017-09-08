package org.jblooming.remoteFile;

import org.jblooming.ontology.Node;


public class BasicDocument extends Document{
  public BasicDocumentBricks bricks = new BasicDocumentBricks(this);

  public void setParent(BasicDocument n) {
     parent = n;
   }

   public void setParentNode(Node node) {
     setParent((BasicDocument) node);
   }

  public BasicDocument getParent() {
    return (BasicDocument) parent;
  }

  public Node getParentNode() {
    return getParent();
  }

}
