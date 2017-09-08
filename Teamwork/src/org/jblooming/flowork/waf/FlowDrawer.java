package org.jblooming.flowork.waf;

import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.view.PageSeed;
import org.jblooming.waf.view.PageState;
import org.jbpm.graph.def.ProcessDefinition;
import org.jbpm.graph.def.Node;
import org.jbpm.graph.exe.ProcessInstance;

public class FlowDrawer extends JspHelper {

  public ProcessDefinition processDefinition;
  public ProcessInstance processInstance;


  public boolean hideStructural=true;

  public int height=600;
  public int width=800;
  public Node focusedNode;

  public FlowDrawer(ProcessDefinition processDefinition, PageState pageState) {
    this(processDefinition, null,pageState);
  }

  public FlowDrawer(ProcessDefinition processDefinition, ProcessInstance processInstance, PageState pageState) {
    this.processInstance = processInstance;
    this.processDefinition = processDefinition;
    urlToInclude = "/commons/flowork/layout/partFlowDrawer.jsp";
  }


}
