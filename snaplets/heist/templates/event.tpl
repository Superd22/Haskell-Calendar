<apply template="base">
    <dfFrom action="/add_event">
    <dfChildErrorList ref="" />

    <dfLabel ref="title">Event Title: </dfLabel>
    <dfInputText ref="title" />
    <br>

    Date:
    <dfSubView ref="Date">
         <dfInputText ref="day" size="2" />
    
         <dfInputText ref="month" size="2" />

         <dfInputText ref="year" size="4" />
    </dfSubView>


    <dfInputSubmit value="Submit" />
   </dfFrom>
</apply>
