{
  "sortable": true,
  // "resizable": true,
  // "filterable": true,
  "groupable": true,
  "pageable": true,
  "navigateable": true,
  // columnMenu: true,
  
  height: 640,
  sortable: true,
  reorderable: true,
  groupable: true,
  resizable: true,
  filterable: true,
  columnMenu: true,
  pageable: true,
  
"columns": [
  {
    "field": "_folder",
    locked: true,
    lockable: false,
    width: 300,
    "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Object</p></div>',
  },
  {
    "field": "_name",
    locked: true,
    lockable: false,
    width: 150,
    "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Tag</p></div>',
  },
  {
    "field": "triggered",
    lockable: false,
    width: 150,
    "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Launched</p></div>',
  },
  {
      field: "task_status",
      locked: true,
      lockable: false,
      width: 150,
      headerTemplate: '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Status</p></div>',
      template: data => {
          let backColor = "gray";
          let fontColor = "black";
          if (data.task_status === "STDBY") {
            backColor = "gray"; 
          } else if (data.task_status === "QUEUED") {
            backColor = "blue";
            fontColor = "white"
          } else if (data.task_status === "PENDING") {
            backColor = "gold";
            fontColor = "black";
          } else if (data.task_status === "SUCCESS") {
            backColor = "green";
            fontColor = "black";
          }else if (data.task_status === "FAILED") {
            backColor = "red";
            fontColor = "gold";
          }
          return `<div style="text-align:center;background-color:${backColor};color:${fontColor}">${data.task_status}</div>`;
      }
  },
  {
    "field": "task_updated",
    lockable: false,
    width: 250,
    "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Updated</p></div>',
  },
  {
    "field": "model_type",
    locked: true,
    lockable: false,
    width: 150,
    "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Type</p></div>',
  },
  {
  "field": "action",
  // "editable": true,
  // title: "Restart",
   "width": 150,
   locked: true,
    lockable: false,
  "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Restart</p></div>',
  "command":[
      {
        name: "Restart",
        visible:function(item){ 
          //   return item.task_status!="STDBY"
          return true
        },
        click:function(e){
            const conn = fp_dev.getConnection();
            e.preventDefault();
            const tr = $(e.target).closest("tr");
            const data = this.dataItem(tr);
            console.log("info", `${data._folder}/${data._name}`);
            
            fp_dev.yesno('Restart the task?', ()=>{
                  conn.edit_or_create_object({
                      ".name": data._name,
                      ".folder": data._folder,
                      ".pattern": "/root/.patterns/model_control",
                      "reset": true,
                  }, ()=>{console.log(`Данные записаны в тэг`);}, ()=>{console.log("Ошибка!")}, 1000);
              });
          }
      }]
  },
  {
  "field": "disable",
  // "editable": true,
  "width": 150,
    lockable: false,
  title: "Disable",
  "headerTemplate": '<div style="font-size: 14px; text-align: center; font-weight: regular";><p style="white-space: normal">Disable</p></div>',
  "command":[
      {
        name: "Disable",
        visible:function(item){ 
            return item.disabled == false
          return true
        },
        click:function(e){
            const conn = fp_dev.getConnection();
            e.preventDefault();
            const tr = $(e.target).closest("tr");
            const data = this.dataItem(tr);
          //   console.log("info", `${data._folder}/${data._name}`);
            
            fp_dev.yesno('Restart the task?', ()=>{
                  conn.edit_or_create_object({
                      ".name": data._name,
                      ".folder": data._folder,
                      ".pattern": "/root/.patterns/model_control",
                      "disabled": true,
                  }, ()=>{console.log(`Данные записаны в тэг`);}, ()=>{console.log("Ошибка!")}, 1000);
              });
          }
      }]
  }
]
}