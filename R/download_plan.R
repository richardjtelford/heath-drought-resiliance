## download data

download_plan <- drake_plan(
  remote_path = "LandPress_Data_for_resiliencepaper/Resilience (temporary)",
  
  #meta data
  meta_download = target(
    get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "drought.plots.xlsx",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "drought.plots.xlsx",
        path = "data"
      )
    )
  ),
  
  #environment
  environment_download = target(
    get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "Environmental variables resilience_2020.xlsx",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "Environmental variables resilience_2020.xlsx",
        path = "data"
      )
    )
  ),
  
  #community data
  community_download = target(
    get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "DE.1_Community.xlsx",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "DE.1_Community.xlsx",
        path = "data"
      )
    )
  ),
  
  #calluna cover data
  calluna_cover_download = target(
    get_file(
      node = "mv84d",
      remote_path = remote_path,
      file = "Datasett resiliens med endringer LGV_29.6.2020.xlsx",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "mv84d",
        remote_path = remote_path,
        file = "Datasett resiliens med endringer LGV_29.6.2020.xlsx",
        path = "data"
      )
    )
  )
  
)
