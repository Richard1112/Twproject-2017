<%@ page import="org.jblooming.flowork.businessLogic.JbpmGraphSupport,
                org.jblooming.flowork.waf.FlowDrawer,
                org.jblooming.utilities.JSP,
                org.jblooming.utilities.ReflectionUtilities,
                org.jblooming.waf.html.core.JspIncluderSupport,
                org.jblooming.waf.html.display.paintable.*,
                org.jblooming.waf.settings.I18n,
                org.jblooming.waf.view.PageState,
                org.jbpm.graph.def.Node,
                org.jbpm.graph.def.ProcessDefinition,
                org.jbpm.graph.def.Transition,
                org.jbpm.graph.node.*,
                org.jbpm.taskmgmt.def.Swimlane,
                org.jbpm.taskmgmt.def.Task, org.jbpm.taskmgmt.exe.TaskInstance, org.jbpm.taskmgmt.exe.TaskMgmtInstance, java.util.*"%>
<%


  FlowDrawer flowDrawer = (FlowDrawer) JspIncluderSupport.getCurrentInstance(request);
  PageState pageState = PageState.getCurrentPageState(request);


  ProcessDefinition def = flowDrawer.processDefinition;
 %><h2><%=def.getName() + " - <small>(ver. " + def.getVersion() + " id:" + def.getId() + ")</small>"%></h2><%

  // forks and joins should be visible??
  boolean hideStructural = flowDrawer.hideStructural;

  //get task with onGoing activities
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

  // get all nodes
  List<Node> allNodes = def.getNodes();
  Folio folio = new Folio(flowDrawer.height, flowDrawer.width);
  folio.id = flowDrawer.id;
  folio.width = 10000;
  folio.height = 10000;
  folio.color = "black";
  //folio.lineSize = 2;
  folio.lineStyle = "solid";
  //folio.style="background-color:#F9EFC5;";

  double labelWidth = 200;
  double labelHeight = 60;

  // creates a nodeTaskMap with a Label for each Node
  Map<Node, Label> mappaNodeLabel = new LinkedHashMap<Node, Label>();
  Map<Label, Node> mappaLabelNode = new LinkedHashMap<Label, Node>();
  for (Node n : allNodes) {
    Node node = (Node) ReflectionUtilities.getUnderlyingObjectAsObject(n);
    // remove fork and join
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

      // determine the color
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


  // get nodes for level
  List<List<Node>> levelNodes= JbpmGraphSupport.getNodesByLevel(def);

  
  // move end state to the last level
  List<Node> whereLevel=null;
  EndState end=null;
  for (List<Node> level : levelNodes) {
    for (Node node : level) {
      if (node instanceof EndState) {
        end= (EndState) node;
        whereLevel=level;
        break;
      }
    }
  }
  if (whereLevel!=null && end!=null){
    whereLevel.remove(end);
    if (whereLevel.size()==0)
      levelNodes.remove(whereLevel);

    LinkedList<Node> lastl=new LinkedList<Node>();
    lastl.add(end);
    levelNodes.add(lastl);
  }


  // set label' y
  double y = -labelHeight;

  for (List<Node> level : levelNodes) {
    y=y+labelHeight * 1.4;
    for (Node node :level) {
      if (mappaNodeLabel.containsKey(node)) {
        Label label = mappaNodeLabel.get(node);
        label.top = y;
      }
    }
  }

  //who many nodes per level?
  int maxNodesPerLevel=0;
  for (List<Node> nodes: levelNodes) {
    maxNodesPerLevel=Math.max(maxNodesPerLevel,nodes.size());
  }
  double maxWidth=maxNodesPerLevel*labelWidth*1.4;

  // set label' x basing on how many nodel per level
  for (List<Node> nodes : levelNodes) {
    double wAvail4Label = maxWidth / nodes.size();

    int i=0;
    for (Node node : nodes) {
      if (mappaNodeLabel.containsKey(node)) {
        Label label = mappaNodeLabel.get(node);
        label.left = (wAvail4Label-label.width)/2 + i*wAvail4Label;
      }
      i++;
    }
  }

  // reset folio' size
  folio.width = maxWidth;
  folio.height = y+labelHeight*1.4;

  // link creation
  for (Node nodeFrom : allNodes) {
    Label labelFrom = mappaNodeLabel.get(nodeFrom);
    if (labelFrom != null) {
      Collection lec = nodeFrom.getLeavingTransitions();
      String lineStyle;
      if (nodeFrom instanceof Fork)
        lineStyle = "solid";
      else if (lec.size() > 1)
        lineStyle = "dashed";
      else
        lineStyle = "solid";

      for (Iterator ite = lec.iterator(); ite.hasNext();) {
        Transition transition = (Transition) ite.next();
        Node dest = transition.getTo();
        String tt = "from '" + nodeFrom.getName() + "' to '" + dest.getName() + (JSP.ex(transition.getName() )? "' value '" + transition.getName() + "'":"");
        // con rimozione dei fork/join
        Set<Node> dess = new HashSet<Node>();

        if (hideStructural) {
          dess = JbpmGraphSupport.getDestinations(dest);
        } else {
          dess = new HashSet<Node>();
          dess.add(dest);
        }

        for (Node nodeTo : dess) {
          int levelFrom= JbpmGraphSupport.getNodelevel(nodeFrom, levelNodes );
          int levelTo= JbpmGraphSupport.getNodelevel(nodeTo, levelNodes );
          Label labelTo = mappaNodeLabel.get(nodeTo);

          if (levelFrom<levelTo){
            //go straight
            Line line = new Line(labelFrom.left + labelFrom.width / 2+levelTo,
                    labelFrom.top + labelFrom.height,
                    (labelTo.left + labelTo.width / 2) - (labelFrom.left + labelFrom.width / 2),
                    labelTo.top - (labelFrom.top+labelHeight));
            line.lineSize=2;
            line.lineStyle=lineStyle;
            line.color="blue";

            //folio.add(line);
            folio.add(new VLink(labelFrom, labelTo, 10, "blue", lineStyle, 2, tt, null));

          }else{
            // go snaked
            folio.add(new Link(labelFrom, labelTo, 20, "red", lineStyle, 2, tt, null));
          }
        }
      }
    }
  }

  // brings labels to front
  for (Label label : mappaNodeLabel.values())
    folio.bringToFront(label);

  // FOLIO draw
  folio.toHtml(pageContext);


  //legenda
  %>
  <table align="center" cellspacing=5 class="legendaProcess">
  <tr>
    <td align="center"><i><%=I18n.get("FLOW_LEGENDA")%>:</i></td>
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
