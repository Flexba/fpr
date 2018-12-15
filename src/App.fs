module App

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

module Browser = Fable.Import.Browser

// MODEL

type IdentifiedToDo =
    string * int

type ToDo =
    | NewTodo of IdentifiedToDo
    | FinishedTodo of IdentifiedToDo

type Model =
    { DraftForm : string
      Drafts : ToDo list }

type Msg =
| UpdateForm of string
| CreateTodo
| FinishTodo of IdentifiedToDo
| DeleteTodo of IdentifiedToDo

let init() : Model =
    { DraftForm = ""
      Drafts = [] }

// UPDATE

let getIdentity(todo:ToDo) : IdentifiedToDo =
    match todo with
    | NewTodo identity ->
        identity
    | FinishedTodo identity ->
        identity

let getTitle(todo:ToDo) : string =
    fst (getIdentity todo)

let finish (identity : IdentifiedToDo) (todo : ToDo) : ToDo =
    match todo with
    | NewTodo newTodo ->
        if newTodo = identity then
            sprintf "Todo %s is finished!" (fst newTodo)
            |> Browser.console.log
            FinishedTodo newTodo
        else todo
    | FinishedTodo _ -> todo

let getIdentity (todo:string) (model:Model) : int = 
    let currentMax = model.Drafts
    |> List.filter (fun x -> getTitle x = todo)
    |> List.map getIdentity
    |> List.map snd
    |> List.max

    currentMax + 1

let update (msg:Msg) (model:Model) =
    match msg with
    | UpdateForm content ->
        { model with DraftForm = content }
    | CreateTodo->
        let newDraft = NewTodo (model.DraftForm, 1)
        { model with
            DraftForm = ""
            Drafts = newDraft::model.Drafts }
    | FinishTodo todo ->
        let drafts = 
            model.Drafts
            |> List.map (finish todo)
        { model with Drafts = drafts }

// VIEW (rendered with React)

open Fulma

let newDraftTile dispatch (title : ToDo) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str (fst title) ] ]
              Card.content []
                [ Content.content [] [ str "Your prestine card draft." ] ]
              Card.footer []
                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> FinishTodo title |> dispatch) ] ]
                    [ str "Bump" ] ] ] ]

let rejectedDraftTile dispatch (title : string) =
    Tile.tile [ Tile.IsChild; Tile.Size Tile.Is4; Tile.CustomClass "content-card" ]
        [ Card.card [ ]
            [ Card.header []
                [ Card.Header.title [] [ str title ] ]
              Card.content []
                [ Content.content [] [ str "Unfortunately this draft has been rejected 🙁" ] ]
              Card.footer []
                [ ] ] ]

let toCard dispatch (draft : ToDo) =
    match draft with
    | NewTodo identity ->
        newDraftTile dispatch (fst identity)
    | NewTodo identity ->
        finishedTodoTile dispatch (fst identity)

let toCardRow row =
    Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ] row

let rec chunkByThree soFar l =
    match l with
    | x1::x2::[x3] ->
        [x1; x2; x3]::soFar
    | x1::x2::x3::xs ->
        chunkByThree ([x1; x2; x3]::soFar) xs
    | xs ->
        xs::soFar

let toCardRows dispatch (titles : ToDo list) =
    titles
    |> chunkByThree []
    |> List.rev
    |> List.map ((List.map (toCard dispatch)) >> toCardRow)

let view (model:Model) dispatch =   
    div []
      [ Navbar.navbar [ Navbar.Color IsBlack ]
            [ Navbar.Brand.div []
                [ Navbar.Item.a [ Navbar.Item.Props [ Href "#" ] ]
                    [ str "Card Manager" ] ] ]
        Container.container [ Container.IsFluid ]
          [ h1 [ Class "is-size-1 app-title" ] [ str "Manage your Cards" ]
            Tile.tile [ Tile.IsAncestor; Tile.IsVertical ]
                [ yield Tile.tile [ Tile.IsParent; Tile.Size Tile.Is12 ]
                    [ Tile.tile [ Tile.IsChild ]
                        [ Card.card []
                            [ Card.header []
                                [ Card.Header.title [] [ str "Write a draft!" ] ]
                              Card.content []
                                [ Input.text [ Input.Placeholder "Your draft"
                                               Input.Value model.DraftForm
                                               Input.OnChange (fun ev -> UpdateForm ev.Value |> dispatch)
                                               Input.Option.Props
                                                 [ (* _4_ there is a OnKeyUp you can use here
                                                      the event holds the id of the pressed key
                                                      you can look up which id corresponds with 'enter'
                                                    *) ] ] ]
                              Card.footer []
                                [ Card.Footer.a [ GenericOption.Props [ OnClick (fun _ -> dispatch CreateTodo) ] ]
                                    [ str "Submit" ] ] ] ] ]
                  yield! model.Drafts |> toCardRows dispatch ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

// App
Program.mkSimple init update view
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
// |> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.run
