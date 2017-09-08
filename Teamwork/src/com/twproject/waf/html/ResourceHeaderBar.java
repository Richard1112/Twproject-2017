package com.twproject.waf.html;

import com.twproject.resource.Resource;
import com.twproject.security.TeamworkPermissions;
import org.jblooming.waf.constants.Commands;
import org.jblooming.waf.html.button.ButtonLink;
import org.jblooming.waf.html.core.JspHelper;
import org.jblooming.waf.html.display.PathToObject;
import org.jblooming.waf.settings.I18n;
import org.jblooming.waf.view.PageSeed;

public class ResourceHeaderBar extends JspHelper {


  public PathToObject pathToObject;
  public Resource resource;

  public ResourceHeaderBar(Resource resource) {
    super();
    this.resource=resource;
    this.urlToInclude = "/applications/teamwork/resource/part/partResourceHeadBar.jsp";

    this.pathToObject = new PathToObject(resource);
    this.pathToObject.canClick = TeamworkPermissions.resource_canRead;
    PageSeed back = new PageSeed("resourceList.jsp");
    this.pathToObject.rootDestination =new ButtonLink(I18n.get("RESOURCES")+" /", back);
  }

}