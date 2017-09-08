<%@ page import="com.twproject.waf.TeamworkPopUpScreen, org.jblooming.waf.ScreenArea, org.jblooming.waf.SessionState, org.jblooming.waf.html.container.Container, org.jblooming.waf.view.PageState"%>
<%
  PageState pageState = PageState.getCurrentPageState(request);
   if (!pageState.screenRunning) {
    pageState.screenRunning = true;

     final ScreenArea body = new ScreenArea(request);
     TeamworkPopUpScreen lw = new TeamworkPopUpScreen(body);
     lw.register(pageState);
     pageState.perform(request, response);

     pageState.toHtml(pageContext);


  } else {
    %><h1>Help</h1>
    <style type="text/css">
        body, tbody{
            line-height: 130%;
        }

        table{
            margin-top: 30px;
        }

      .help {
        background-color:#F9EFC5;
        padding:6px;
      }



       #help td {
        text-align: left;
        vertical-align: top;
        color: #777;
           font-size: 95%;
  }
      .formElements {
        font-size: 14px;
        padding: 1px;
      }

        #help td.codeop {
          font-weight: bold;
          text-align: center;
          vertical-align: middle;
          color: #2f2f2f;
          font-size: 90%;

        }

        table.edged > tbody > tr > td:last-of-type {
            padding-left: 10px;
        }

    </style>

      <p>The query by example search method gives the user an easy way to compose complex queries,
      by using a particular syntax in the search fields.
      If for example in a field you write "$Mixer*" and click the search button,
      you will get all results that start with "Mixer".
      If values are specified in more then one field, all these must be satisfied (fields are in "AND").
      Queries can be composed with the following parameters:</p>

<table cellpadding="3" id="help" class="table edged">
    <tr>
        <th class="tableHead">Condition</th>
        <th width="1%" class="tableHead">Operator</th>
        <th class="tableHead">Example</th>
    </tr>

    <tr>
        <td>comparison</td>
        <td class="codeop">=,&gt;,&lt;,&gt;=,&lt;=</td>
        <td>All tasks whose progress is above 5%:&nbsp;&nbsp;
          <span class="help">
            progress: <input type="text" readonly class="formElements" value=">5" size=3>%
          </span>

            More complex: above 3, lesser than 6 or equal to 10:
      <span class="help">
            unitary price: <input type="text" readonly class="formElements" value=">3+<6|=10" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>not equal</td>
        <td class="codeop">!</td>
        <td>All tasks whose description does not contain "jewel": &nbsp;&nbsp;
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="!jewel" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>exactly equal</td>
        <td class="codeop">""</td>
        <td>All tasks whose description is "nice jewel", excluding those that have only "nice" or only "jewel":
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="&quot;jewel&quot;" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>empty field</td>
        <td class="codeop">(), []</td>
        <td>All tasks whose description and name are "null": &nbsp;&nbsp;
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="()" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>zero length field</td>
        <td class="codeop">//</td>
        <td>All tasks whose description and name have empty content: &nbsp;&nbsp;
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="//" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>not empty field</td>
        <td class="codeop">!(), ![]</td>
        <td>All tasks whose description and name are not empty: &nbsp;&nbsp;
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="!()" size=10>
          </span>
        </td>
    </tr>

    <tr>
        <td>contains</td>
        <td class="codeop">*</td>
        <td>All tasks whose description or name end with "spies": &nbsp;&nbsp;
        <span class="help">
          name/description <input type="text" readonly class="formElements" value="*spies" size=10>
        </span>
            All tasks whose description or name start with "Contact": &nbsp;&nbsp;
        <span class="help">
          name/description <input type="text" readonly class="formElements" value="Contact*" size=10>
        </span>
            All tasks whose description or name contain "an":&nbsp;&nbsp;
        <span class="help">
          name/description <input type="text" readonly class="formElements" value="*an*" size=10>
        </span>
        </td>
    </tr>

    <tr>
        <td>isolated word</td>
        <td class="codeop">#</td>
        <td>All tasks whose description or name contain "jewel" as isolated word: &nbsp;&nbsp;
          <span class="help">
            name/description <input type="text" readonly class="formElements" value="#jewel" size=10>
          </span>
            Will find "Another jewel robbery" but not "Jewellery stolen"

        </td>
    </tr>

    <tr>
        <td>is between</td>
        <td class="codeop">:</td>
        <td>All tasks whose start is between the dates below: &nbsp;&nbsp;
          <span class="help">
            start <input type="text" readonly class="formElements" value="10/08/2005:20/12/2006" size=25>
          </span>
        </td>
    </tr>

    <tr>
        <td>conjunction</td>
        <td class="codeop">+</td>
        <td>All tasks whose description or name contain "another" AND contain "jewel":
        <span class="help">
          name/description <input type="text" readonly class="formElements" value="another+jewel" size=20>
        </span>
        </td>
    </tr>

    <tr>
        <td>disjunction</td>
        <td class="codeop">| ,</td>
        <td>All tasks whose description or name contain "another" OR contain "jewel":
            <span class="help">
              name/description <input type="text" readonly class="formElements" value="another|jewel" size=20>
            </span>
            or
              <span class="help">
              name/description <input type="text" readonly class="formElements" value="another,jewel" size=20>
            </span>

        </td>
    </tr>

    <tr>
        <td>Parametric date settings</td>
        <td class="codeop">YESTERDAY<br>TODAY<br>TOMORROW<br>LQ LM LW Y T NW NM NQ<br>(-)<i>n</i>[DWMY]</td>
        <td>These constants get substituted in searches with their current values: &nbsp;&nbsp;<br>
            Finds all events started after yesterday. &nbsp;&nbsp;
      <span class="help">
          start <input type="text" readonly class="formElements" value=">YESTERDAY" size=10>
        </span>
            <br>

            Finds all events ending between one week ago and three months. &nbsp;&nbsp;
      <span class="help">
        end <input type="text" readonly class="formElements" value="-1w:3m" size=10>
      </span>


        </td>
    </tr>

</table>
<br>
<strong class="block pTL-10">There is a syntax also for date shortcuts, see the user guide.</strong>

In order to save a filter just write a name in the "filter name" field, and press "search". From then on, on your profile the filter will be available in the combo.
Notice also that also the column ordering chosen will be preserved.<br>
If the filter's name starts with "d:", if will be applied by default on accessing the search page.

   <%
   }
%>
