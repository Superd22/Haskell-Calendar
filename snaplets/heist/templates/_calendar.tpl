<body>
    <div align= "right">
        <p><loggedInUser/> <a href="/logout">(Logout)</a></p>
    </div>
    <div align= "center">
        <span style="padding-right:250px">
            <a href="/calendar/pyear">Prev Year</a>
            <a href="/calendar/pmonth">Prev Month</a>
        </span>
        <a href="/add_event">Add Event</a>
        <span style="padding-left:250px">
            <a href="/calendar/nmonth">Next Month</a>
            <a href="/calendar/nyear">Next Year</a>
        </span>
        <calendar>
        <table class="cal">
            <caption><month/></caption>
            <tr class="weekdays">
                <td class ="calcol">Monday</td> 
                <td class ="calcol">Tuesday</td>
                <td class ="calcol">Wednesday</td>
                <td class ="calcol">Thursday</td>
                <td class ="calcol">Friday</td>
                <td class ="calcol">Saturday</td>
                <td class ="calcol">Sunday</td>
            </tr>
            <week>
            <tr class= "calrow">
                <data>
                <td class= "calcol"><day/>
                <br>
                <event>
               <h2>
               <etitle/><arrow/><hour/><sep/><minutes/><delete/>
               </h2>
                </event>
                </td>
                </data>
            </tr>
            </week>
        </table>
    </calendar>
    </div>
</body>  
