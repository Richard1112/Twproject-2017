<%@ page import="org.jblooming.flowork.businessLogic.JbpmGraphSupport,
                org.jblooming.flowork.waf.FlowDrawer,
                org.jblooming.utilities.JSP,
                org.jblooming.utilities.ReflectionUtilities,
                org.jblooming.waf.html.core.JspIncluderSupport,
                org.jblooming.waf.html.display.paintable.Folio,
                org.jblooming.waf.html.display.paintable.HLine,
                org.jblooming.waf.html.display.paintable.Label,
                org.jblooming.waf.html.display.paintable.Link,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageState,
                org.jbpm.graph.def.Node,
                org.jbpm.graph.def.ProcessDefinition,
                org.jbpm.graph.def.Transition,
                org.jbpm.graph.node.*,
                org.jbpm.taskmgmt.def.Swimlane,
                org.jbpm.taskmgmt.def.Task,
                org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.*"%>
<%


  FlowDrawer flowDrawer = (FlowDrawer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


  ProcessDefinition def = flowDrawer.processDefinition;


  %><h2><%=def.getName() + " - <small>(ver. " + def.getVersion() + " id:" + def.getId() + ")</small>"%></h2><%

  // si devono far vedere i fork e i join?
  boolean hideStructural = flowDrawer.hideStructural;

  //si prendono i task eseguiti che al loro interno hanno i sospesi
  List<Node> completedNodes = new ArrayList<Node>();
  HashMap<Node, TaskInstance> nodeTaskMap = new HashMap<Node, TaskInstance>();
  if (flowDrawer.processInstance != null) {
    TaskMgmtInstance tmi = flowDrawer.processInstance.getTaskMgmtInstance();
    if (tmi.getTaskInstances() != null && tmi.getTaskInstances().size() > 0) {
      for (Object o : tmi.getTaskInstances()) {
        TaskInstance taskIns = (TaskInstance) o;
        Node node = taskIns.getTask().getTaskNode();
        completedNodes.add(node);
        nodeTaskMap.put(node, taskIns);
      }
    }
  }

  // si prendono tutti il nodi
  List<Node> allNodes = def.getNodes();
  Folio folio = new Folio(flowDrawer.height, flowDrawer.width);
  folio.width = 10000;
  folio.height = 10000;
  folio.lineStyle = "solid";

  double labelWidth = 100;
  double labelHeight = 40;

  // si crea una nodeTaskMap con una Label per ogni nodo
  Map<Node, Label> mappaNodeLabel = new LinkedHashMap<Node, Label>();
  Map<Label, Node> mappaLabelNode = new LinkedHashMap<Label, Node>();
  for (Node n : allNodes) {
    Node node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(n);
    // si rimuovono fork e join
    if (hideStructural && (node instanceof Fork || node instanceof Join)) {
    } else {
      String labelValue=node.getName()+"<br><small><small>";
      if (node instanceof TaskNode) {
        TaskNode tn = (TaskNode) node;
        if (JSP.ex(tn.getDescription()))
          labelValue= labelValue+JSP.encode(tn.getDescription())+"<hr>";
        Set<Task> tset = tn.getTasks();
        for (Task task : tset) {

          Swimlane swimlane = task.getSwimlane();
          String descr="";
          if (swimlane==null)
            descr= "Delegation: "+task.getAssignmentDelegation().getInstance().getClass().getSimpleName()+ "<br>"+JSP.htmlEncode(task.getAssignmentDelegation().getConfiguration());
           else
            descr=swimlane.getName()+JSP.w(task.getName().equalsIgnoreCase(node.getName())?"":": "+task.getName());
                    
          labelValue= labelValue+"<b>" + descr +"</b><br>";
          if (JSP.ex(task.getDescription()))
            labelValue= labelValue+ JSP.encode(task.getDescription())+"<br>";
        }
      }
      labelValue=labelValue+"</small></small>";

      Label label = new Label(labelHeight, labelWidth, 0, 0, labelValue);
      if (completedNodes.contains(node)) {
        TaskInstance taskInstance = nodeTaskMap.get(node);
        if (taskInstance.hasEnded()){
          label.backgroundColor = pageState.getColor("FLOW_BG_COMPLETED");
          label.color = pageState.getColor("FLOW_BORDER_COMPLETED");
        } else{
          label.color = pageState.getColor("FLOW_BORDER_ACTIVE");
          label.backgroundColor = pageState.getColor("FLOW_BG_ACTIVE");
        }

      } else{
        if (node instanceof StartState) {
          label.backgroundColor = pageState.getColor("FLOW_BG_TERMINAL");
        }
        else if (node instanceof EndState)
          label.backgroundColor =pageState.getColor("FLOW_BG_TERMINAL");
        else if (node instanceof MilestoneNode)
          label.backgroundColor = pageState.getColor("FLOW_BG_MILESTONE");
        else if (node instanceof Fork) {
          label.width = labelWidth / 2;
          label.backgroundColor = pageState.getColor("FLOW_BG_FORKJOIN");
        } else if (node instanceof Join) {
          label.width = labelWidth / 2;
          label.backgroundColor = pageState.getColor("FLOW_BG_FORKJOIN");
        } else
          label.backgroundColor = pageState.getColor("FLOW_BG_DEFAULT");
        label.color = pageState.getColor("FLOW_BORDER_DEFAULT");
      }
      // check for focused node
      if (flowDrawer.focusedNode!=null && flowDrawer.focusedNode.getId()==node.getId())
        label.color=pageState.getColor("FLOW_BORDER_FOCUSED");

      label.lineSize = 2;
      label.lineStyle = "solid";
      label.style = "overflow:auto;opacity:0.9; filter:alpha(opacity=90);";
      label.script="nodeId=\""+node.getId()+"\"";
      mappaNodeLabel.put(node, label);
      mappaLabelNode.put(label, node);
      folio.add(label);
    }
  }

  // si crea una nodeTaskMap di swimlane, list di label
  LinkedHashMap<String, List<Label>> mappaSwimlanes = new LinkedHashMap<String, List<Label>>();
  for (Node node : mappaNodeLabel.keySet()) {
    String swim = JbpmGraphSupport.getSwimlane(node);
    //String swim = "dummy";
    if (!mappaSwimlanes.containsKey(swim))
      mappaSwimlanes.put(swim, new ArrayList<Label>());
    mappaSwimlanes.get(swim).add(mappaNodeLabel.get(node));
  }

  // si crea una lista di swimlanes per beccare la riga
  //List<String> sws = new LinkedList();
  //sws.addAll(mappaSwimlanes.keySet());

  // si posizionano le y delle label progressivamente
  double y = labelHeight / 8;
  for (String swim : mappaSwimlanes.keySet()) {
    Label swl = new Label(labelHeight, labelWidth, y, folio.left + 1, swim); 
    swl.color = "#d0d0d0";
    folio.add(swl);

    List<Label> ll = mappaSwimlanes.get(swim);
    for (Label label : ll) {
      Node node = mappaLabelNode.get(label);
      if (node instanceof Join) { // in modo che i join siano allineati con chi li segue
        label.top = y;
      } else if (node instanceof Fork) {
        label.top = y - labelHeight * 1.1;
      } else {
        label.top = y;
        y = y + labelHeight * 1.1;
      }
    }
    y = y + labelHeight / 8;
    HLine hl = new HLine(folio.width, y, folio.left);
    hl.color = "#e0e0e0";
    folio.add(hl);
    y = y + labelHeight / 8;
  }

  // poi si posizionano le x della label sulla base dell'ordine di esecuzione
  double x = 40;
  for (Node node : allNodes) {
    if (mappaNodeLabel.containsKey(node)) {
      Label label = mappaNodeLabel.get(node);

      if (node instanceof Fork) { // in modo che i Fork siano allineati con chi li segue
        x = x + labelWidth * .7;
        label.left = x;
        x = x + labelWidth * .5;
      } else if (node instanceof Join) {
        label.left = x;
        x = x + labelWidth * .7;
      } else {
        label.left = x;
        x = x + labelWidth * .5;
      }

    }
  }

 

  // si aggiorna la dimensione del foglio
  folio.width = x + labelWidth;
  folio.height = y;

  // si creano i link
  for (Node node : allNodes) {
    Label labelFrom = mappaNodeLabel.get(node);
    if (labelFrom != null) {

      Collection lec = node.getLeavingTransitions();


      String lineStyle;
      if (node instanceof Fork)
        lineStyle = "solid";
      else if (lec.size() > 1)
        lineStyle = "dashed";
      else
        lineStyle = "solid";

      for (Iterator ite = lec.iterator(); ite.hasNext();) {
        Transition transition = (Transition) ite.next();
        Node dest = transition.getTo();
        //if (dest != null) {
        String tt = "from '" + node.getName() + "' to '" + dest.getName() + "' value '" + transition.getName() + "'";
        // con rimozione dei fork/join
        Set<Node> dess = new HashSet<Node>();

        if (hideStructural) {
          dess = JbpmGraphSupport.getDestinations(dest);
        } else {
          dess = new HashSet<Node>();
          dess.add(dest);
        }

        for (Node finalDest : dess) {
          folio.add(new Link(labelFrom, mappaNodeLabel.get(finalDest), 10, "blue", lineStyle, 1, tt, null));
        }
      }
      //}
    }
  }

  // si portano le label in primo piano
  for (Label label : mappaNodeLabel.values())
    folio.bringToFront(label);

  // SI DISEGNA FOLIO
  folio.toHtml(pageContext);


  //legenda
  %>
  <table align="center" cellspacing=5 class="legendaProcess">
  <tr>
    <td align="center"><i style="color: #959595; margin-right: 12px"><%=I18n.get("FLOW_LEGENDA")%>:</i></td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_ACTIVE")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_ACTIVE")%>;">&nbsp;</td><td><%=I18n.get("FLOW_ACTIVED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_DEFAULT")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_FOCUSED")%>;">&nbsp;</td><td><%=I18n.get("FLOW_FOCUSED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_COMPLETED")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_COMPLETED")%>;">&nbsp;</td><td><%=I18n.get("FLOW_COMPLETED_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_DEFAULT")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_DEFAULT")%>;">&nbsp;</td><td><%=I18n.get("FLOW_TOBERUN_LEGENDA")%>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_TERMINAL")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_DEFAULT")%>;">&nbsp;</td><td><%=I18n.get("FLOW_TERMINAL_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_MILESTONE")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_DEFAULT")%>;">&nbsp;</td><td><%=I18n.get("FLOW_MILESTONE_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>
    <td class="legendaColors" style="background-color:<%=pageState.getColor("FLOW_BG_FORKJOIN")%>; border:1px solid <%=pageState.getColor("FLOW_BORDER_DEFAULT")%>;">&nbsp;</td><td><%=I18n.get("FLOW_FORKJOIN_LEGENDA")%>&nbsp;&nbsp;&nbsp;</td>



  </tr>
  </table><%



%>