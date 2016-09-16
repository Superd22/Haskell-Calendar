<apply template="base">
    <dfForm action="/add_event">
    <dfChildErrorList ref="" />

    <dfLabel ref="title">Event Title: </dfLabel>
    <dfInputText ref="title" />
    <br>

    Date:
    <dfSubView ref="date">
        <apply template="date" />
    </dfSubView>
    <br>
    Time:
    <dfSubView ref="time">
        <apply template="time" />
    </dfSubView>
    <br>
    <dfLabel ref="repeat">Repeat the event: </dfLabel>
    <dfInputSelect ref="repeat" />
    <br>
    <dfInputSubmit value="Submit" />
   </dfForm>
  <a href="/calendar">Back</a> 
</apply>
