<%@ page import="com.twproject.waf.TeamworkPopUpScreen,
                 org.jblooming.waf.ScreenArea,
                 org.jblooming.waf.view.PageState" %><%@ page pageEncoding="UTF-8" %>
<%
  PageState pageState = PageState.getCurrentPageState(request);

  if (!pageState.screenRunning) {
    pageState.screenRunning = true;
    //put controller !
    final ScreenArea body = new ScreenArea(request);
    TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
    lw.register(pageState);
    pageState.perform(request, response).toHtml(pageContext);

  } else {

    %><p><br>
Twproject is developed using several libraries and quoting several software products and standards.
Here we report their trademarks. We did all efforts to quote all the used products;
if your is missing, just write us at info@twproject.com and we'll add you. Thanks!
<br>
Adobe®, the Adobe logo, Acrobat®, ReaderTM, and Macromedia Flash PlayerTM are either registered trademarks or trademarks of
        Adobe Systems Incorporated in the United States and/or other countries. <br>
Sun, Sun Microsystems, Solaris, Java, JavaServer Web Development Kit, and JavaServer Pages are trademarks or registered trademarks of Sun Microsystems, Inc. <br>
UNIX is a registered trademark in the United States and other countries, exclusively licensed through X/Open Company, Ltd. <br>
Mozilla is © 1998- 2007 2007 by Contributors to the Mozilla codebase under the Mozilla Public License and Netscape Public License. <br>
Netscape® is a registered trademark of Netscape Communications Corporation in the United States and other countries.
        Netscape CommunicatorTM is a trademark of Netscape Communications Corporation that may be registered in other countries.<br>
Microsoft®, Windows®, Windows NT®, SQL Server®, Microsoft Project®, Outlook® and Internet Explorer® are registered
        trademarks of Microsoft Corporation.<br>
Oracle® is a registered trademark of Oracle Corporation.<br>
Informix® is a registerd trademark owned by IBM (International Business Machines Corporation) <br>
Sybase® is a trademark of Sybase Inc.<br>
FrontBase" is a trademark of FrontBase, Inc<br>
Interbase® is a registered trademark of Borland® Copyright© 1994- 2007 2006 Borland Software Corporation<br>
MySQL is © 1995- 2007 2006 MySQL AB under the free software/open source GNU General Public License (GPL). <br>
PostgreSQL is copyright © 1996-2002 by The PostgreSQL Global Development Group<br>
Suse is a trademark of Novell Corporation, Fedora, Red Hat, Red Hat Enterprise Linux of Red Hat Corporation.<br>
SvnKit® is a registered trademark of TMate Software®<br>  
This product includes  software developed  by the  Apache Software Foundation  (http://www.apache.org/).<br>
The names and logos for Basecamp are registered trademarks of 37signals, LLC..<br>

<br>
Ical4J is Copyright (c) 2006, Ben Fortuna<br>
All rights reserved. See http://ical4j.sourceforge.net <br>
This product includes software developed by the Apache Software
Foundation (http://www.apache.org/).

<br>
For language guessing:<br>
TCatNG (http://tcatng.sourceforge.net), is released under the BSD License (http://www.opensource.org/licenses/bsd-license.php).
<br>

<br>
All other brand and product names appearing on this application may be the trademarks or service marks of their respective owners. <br>
See all the license files contained in the WEB-INF/licenses folder.
</p>
      <%


  }
%>

