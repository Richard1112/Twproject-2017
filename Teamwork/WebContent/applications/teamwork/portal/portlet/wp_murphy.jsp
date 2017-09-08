<%@ page import="org.jblooming.utilities.JSP, org.jblooming.waf.html.container.Container, org.jblooming.waf.settings.ApplicationState, org.jblooming.waf.view.PageState, org.w3c.dom.Document, org.w3c.dom.NamedNodeMap, org.w3c.dom.Node,  org.w3c.dom.NodeList, javax.xml.parsers.DocumentBuilder, javax.xml.parsers.DocumentBuilderFactory, java.util.ArrayList, java.util.List" %>
<%!
  public class MurphyLaw {
    public String section;
    public String law;

    public MurphyLaw(String section, String law) {
      this.section=section;
      this.law=law;
    }
  }
%><div class="portletBox"><%


  PageState pageState = PageState.getCurrentPageState(request);
  List<MurphyLaw> murphs = (List<MurphyLaw>) ApplicationState.applicationParameters.get("MURPHY");
  if (murphs == null) {
    murphs = new ArrayList<MurphyLaw>(); 

    String murphyfile = request.getRealPath("/applications/teamwork/portal/portlet/murphy.xml");

    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

    //Using factory get an instance of document builder
    DocumentBuilder db = dbf.newDocumentBuilder();

    //parse using builder to get DOM representation of the XML file
    Document document = db.parse(murphyfile);
    NodeList nodes = document.getElementsByTagName("law");
    for (int i = 0; i < nodes.getLength(); i++) {
      Node law = nodes.item(i);
      Node section = (Node)law.getParentNode();
      NamedNodeMap nnm = section.getAttributes();
      String sectionName = nnm.getNamedItem("name").getNodeValue();
      Node child = law.getFirstChild();
      if (child==null)
        continue;
      String lawText = child.getNodeValue();
      murphs.add(new MurphyLaw(sectionName, lawText));
    }

    ApplicationState.applicationParameters.put("MURPHY", murphs);

  }
  MurphyLaw law = murphs.get((int) (Math.random() * murphs.size()));

%><h1><%=law.section%></h1><p><%=JSP.convertLineFeedToBR(law.law)%></p></div>