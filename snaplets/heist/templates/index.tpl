<apply template="base">

  <ifLoggedIn>
    <meta http-equiv="refresh" content="0; url=/calendar" />
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
