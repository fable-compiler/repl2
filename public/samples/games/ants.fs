module Mario

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

module Types =

    open System

    let xSize = 15
    let ySize = 15
    let nestSize = 3
    let maxTotalFoodPerSquare = 200
    let minGeneratedFoodPerSquare = 10
    let maxGeneratedFoodPerSquare = 50
    let maxFoodAntCanCarry = 5
    let chanceOfFood = 0.12

    let spawnFood = 50
    let maxAntWounds = 2

    let maxCellPheromoneQuantity = 255
    let maxAntDropPheromoneQunatity = 50
    let pheromoneDispersalRate = 1

    let percentFoodToWin = 0.5
    let maxWorldCycles = 1500

    type UID = { X: int; Y: int }
    let uid (x, y) = { X = x; Y = y}

    //type PheromoneType =
    //    | Sweet = 0 
    //    | Sour = 1
    //    | Gross = 2

    type AntColor =
        | Black
        | Red
        with
            member t.Other = 
                match t with
                | Black -> Red
                | Red -> Black
                
    type WorldCellType =
            | FieldCell
            | NestCell of AntColor

    [<Struct>] 
    type Ant =
        val Color: AntColor
        val FoodCarried: int
        val Wounds: int
        new (color) = { Color = color; FoodCarried = 0; Wounds = 0}
        new (color, food) = { Color = color; FoodCarried = food; Wounds = 0 }
        new (color, food, wounds) = { Color = color; FoodCarried = food; Wounds = wounds }
        member internal x.UpdateFood newFood = new Ant(x.Color, newFood)
        member internal x.UpdateWounds newWounds = new Ant(x.Color, x.FoodCarried, newWounds)
        member x.IsFullOfFood = x.FoodCarried >= maxFoodAntCanCarry
        member x.HasFood = x.FoodCarried > 0
        member x.MaxPheromonesToDrop = maxAntDropPheromoneQunatity

    and WorldCell = {
        Id : UID
        Food : int
        Ant : option<Ant> 
        CellType : WorldCellType
        Pheromones : Map<AntColor, int> }
        with
            member t.IsFullOfFood = t.Food >= maxTotalFoodPerSquare

    and TheWorld = Map<UID, WorldCell>    

    type WorldChange = TheWorld -> TheWorld

    [<Struct>] 
    type Nest(ix: int, iy: int, sizex: int, sizey: int) =
        member internal t.MinX = ix
        member internal t.MinY = iy
        member internal t.MaxX = ix + sizex
        member internal t.MaxY = iy + sizey  
        member internal t.IsInBounds x y = x >= t.MinX && x <= t.MaxX && y >= t.MinY && y <= t.MaxY
        member t.Distance cell =
                let cx, cy = t.MinX + ((t.MaxX - t.MinX) / 2), t.MinY + ((t.MaxY - t.MinY) / 2)
                let x, y = cell.Id.X, cell.Id.Y
                let pow x = x * x 
                sqrt (pow(double cx - double x) + pow(double cy - double y))
        member t.CountFood (world: TheWorld) = 
                let t = t in Map.fold (fun s (k: UID) v -> if t.IsInBounds k.X k.Y then s + v.Food else s) 0 world   
        member internal t.CellsWithMaxFood (world: TheWorld) = 
                let t = t in
                    Map.filter (fun (k: UID) v -> t.IsInBounds k.X k.Y) world   
                    |> Map.toList
                    |> List.filter (fun (k,v) -> v.IsFullOfFood)


module UserTypes =

    open Types

    /// Represents an ant's view of another ant (or him/her self)
    [<Struct>]
    type AntView (ant: Ant, viewingAnt: Ant) = 
        /// The amount of food carried by this ant
        member t.FoodCarried = ant.FoodCarried
        /// True if the ant is carrying food, false otherwise
        member t.CarryingFood = ant.FoodCarried > 0
        /// True if the ant is carrying it's maximum amount of food, false otherwise
        member t.CarryingMaxFood = ant.FoodCarried = maxFoodAntCanCarry
        /// True if the ant is wounded, false otherwise
        member t.IsWounded = ant.Wounds > 0
        /// True if the ant is an enemy, false otherwise
        member t.IsEnemy = ant.Color <> viewingAnt.Color

    /// Represents an ant's view of a particular cell
    [<Struct>] 
    type AntCellView (wc: WorldCell, ant: Ant, nest: Nest) =
        /// True when the cell cannot contain any more food, false otherwise
        member t.IsFullOfFood = wc.Food >= maxTotalFoodPerSquare
        /// True when the cell contains some food, false otherwise
        member t.HasFood = wc.Food > 0
        /// The amount of food the cell contains. 0 if none
        member t.FoodContained = wc.Food
        /// True when the cell contains an enemy ant, false otherwise
        member t.ContainsEnemyAnt = match wc.Ant with | Some wcant -> wcant.Color <> ant.Color | None -> false
        /// True the the cell contains any ant, false otherwise
        member t.ContainsAnt = wc.Ant.IsSome
        /// The ant occupying this cell, if any
        member t.Ant = let ant = ant in wc.Ant |> Option.map (fun a -> AntView(a, ant))
        /// True if the cell contains a friendly pheromone signal, false otherwise
        member t.HasFriendlyPheromone = not (wc.Pheromones.[ant.Color] = 0)
        /// Returns the quantity of friendly pheromone in this cell, 0 if none
        member t.FriendlyPheromoneQuantity = wc.Pheromones.[ant.Color]
        /// True if the cell contains an enemy pheromone, false otherwise
        member t.HasEnemyPheromone = not (wc.Pheromones.[ant.Color.Other] = 0)
        /// Returns the quantity of enemy pheromone in this cell, 0 if none
        member t.EnemyPheromoneQuantity = wc.Pheromones.[ant.Color.Other]
        /// Returns the maximum amount of pheromones this cell can contain, 0 is always the minimum
        member t.MaxPheromones = maxCellPheromoneQuantity
        /// Returns the maximum amount of food this cell can contain, 0 is always the minimum
        member t.MaxFood = maxTotalFoodPerSquare    
        /// True if the cell is a friendly nest cell, false otherwise
        member t.IsMyNest = wc.CellType = WorldCellType.NestCell(ant.Color)
        /// True if the cell is an enemy nest cell, false otherwise
        member t.IsEnemyNest = wc.CellType = WorldCellType.NestCell(ant.Color.Other)
        /// Returns the distance in cells from this cell to the your friendly nest
        member t.DistanceToNest = if wc.CellType = WorldCellType.NestCell(ant.Color) then 0.0 else nest.Distance wc
        member internal t.WorldCell = wc

    /// Represents an ant's view of surrounding cells.
    [<Struct>] 
    type AntNearbyView (cells: AntCellView list) = 
        static member internal FromWorldCells worldcells ant nest = worldcells |> List.map (fun c -> AntCellView(c, ant, nest)) |> (fun acvs -> AntNearbyView acvs)
        /// A list of all neighboring cells
        member t.Cells = cells
        /// A list of neighboring cells which do not contain ants
        member t.EmptyCells = cells |> List.filter (fun c -> not c.ContainsAnt)
        /// A list of neighboring cells which contain enemy ants
        member t.EnemyCells = cells |> List.filter (fun c -> c.ContainsEnemyAnt)
        /// A list of neighboring cells which are part of the friendly nest
        member t.MyNestCells = cells |> List.filter (fun c -> c.IsMyNest)
        /// A list of neighboring cells which are part of the enemy nest
        member t.EnemyNestCells = cells |> List.filter (fun c -> c.IsEnemyNest)
        /// A list of neighboring cells which contain food but are not part of the friendly nest 
        member t.FoodCollectionCells = cells |> List.filter (fun c -> not c.IsMyNest && c.HasFood)
        /// A list of neighboring cells which contain friendly pheromones and no ants
        member t.FriendlyPheromoneCells = cells |> List.filter (fun c -> not c.ContainsAnt && c.HasFriendlyPheromone)
        /// A list of neighboring cells which are nest cells and are not full of food
        member t.FoodDropCells = t.MyNestCells |> List.filter (fun c -> not c.IsFullOfFood)

    type AntAction =
        | Nothing
        | Move of AntCellView
        | TakeFood of AntCellView
        | DropFood of AntCellView
        | DropPheromone of AntCellView * int
        | Attack of AntCellView


    type IAntBehavior =
        abstract member Name : string
        abstract member Behave : AntView -> AntCellView -> AntNearbyView -> AntAction     

module Helpers =

    open System
    open System.Reflection

    module Array =
        let randomPermute a =
            let n = Array.length a
            if n > 0 then
                let rand = new Random()
                let rec aux = function
                    | 0 -> a
                    | k ->
                        let i = rand.Next(k+1)
                        let tmp = a.[i]
                        a.[i] <- a.[k]
                        a.[k] <- tmp
                        aux (k-1)
                aux (n-1)
            else a

    module Seq = 
        let randomPermute a =
            a |> Seq.toArray |> Array.randomPermute |> Array.toSeq

    module List = 

        let private r = Random(int DateTime.Now.Ticks)
        let random l =
            let index = r.Next(0, List.length l) in
                l.[index]

module World =

    open System

    open Types
    open Helpers
    open UserTypes

    let BlackAntNest = new Nest( 0, 0, nestSize - 1, nestSize - 1 )
    let RedAntNest = new Nest( 1 + xSize - nestSize, 1 + ySize - nestSize, nestSize - 1, nestSize - 1)

    let (|InBlackNest|InRedNest|Neither|) (x,y) = 
        if BlackAntNest.IsInBounds x y then InBlackNest
        elif RedAntNest.IsInBounds x y then InRedNest
        else Neither

    let getAntNest (ant: Ant) =
        match ant.Color with
        | AntColor.Black -> BlackAntNest
        | AntColor.Red -> RedAntNest

    let emptyPheromoneSet = Map.ofSeq 
                            <| seq { let colors = [| AntColor.Black; AntColor.Red |]
                                    for color in colors do 
                                        yield color, 0 }

    let defaultCell id = {Id = id; Food = 0; Ant = None; CellType = FieldCell; Pheromones = emptyPheromoneSet }
    let defaultBlackAnt = Some <| Ant(AntColor.Black, 0)
    let defaultRedAnt = Some <| Ant(AntColor.Red, 0)

    let buildWorldInitialWorld () =
        let rnd = new System.Random() in 
            Map.ofSeq <|
            seq { for x in 0 .. xSize do
                    for y in 0 .. ySize do
                        let uid = uid (x, y)
                        let defaultcell = defaultCell uid
                        match x, y with
                        | InBlackNest -> yield uid, { defaultcell with Ant = defaultBlackAnt; CellType = NestCell(AntColor.Black) }
                        | InRedNest ->   yield uid, { defaultcell with Ant = defaultRedAnt; CellType = NestCell(AntColor.Red) }
                        | Neither ->     if chanceOfFood > rnd.NextDouble()
                                            then yield uid, { defaultcell with Food = rnd.Next(minGeneratedFoodPerSquare, maxGeneratedFoodPerSquare) }
                                            else yield uid, defaultcell 
                }

    let getAntViews (world: TheWorld) = 
        let getWorldCell x y = Map.tryFind (uid (x,y)) world
        Map.fold
            (fun state (uid: UID) cell ->
                let x, y = (uid.X, uid.Y)
                match cell.Ant with
                | None -> state
                | Some(ant) ->
                    let visibleCells = [ getWorldCell x (y - 1); getWorldCell x (y + 1); getWorldCell (x - 1) y; getWorldCell (x + 1) y ]
                                    |> List.choose id
                    state @ [ant, cell, visibleCells, getAntNest ant])
            [] world

    let getAntActions (bBehave: IAntBehavior) (rBehave: IAntBehavior) (views: (Ant * WorldCell * WorldCell list * Nest) list) =
        let getAntBehavior (ant: Ant) =
            match ant.Color with
            | AntColor.Black -> bBehave
            | AntColor.Red -> rBehave
        views |> List.map (fun (ant, cell, antView, nest) -> let behavior = getAntBehavior ant in 
                                                                cell, behavior.Behave (AntView(ant, ant)) (AntCellView (cell, ant, nest)) (AntNearbyView.FromWorldCells antView ant nest))
    let buildDependentTransaction (expectedCells: WorldCell list) actions = 
        let predicate = (fun (world: TheWorld) -> expectedCells |> List.forall (fun (cell: WorldCell) -> (Map.find cell.Id world) = cell))
        let action = (fun (iworld: TheWorld) -> 
            List.fold (fun (cworld: TheWorld) (id, action) ->
                Map.add id (action cworld.[id]) cworld) iworld actions)
        predicate, action  

    let dropPheromonesInTargetCell antColor quantity target = 
        let newValue = max (target.Pheromones.[antColor] + quantity) maxCellPheromoneQuantity in 
            { target with Pheromones = target.Pheromones.Add(antColor, newValue ) } 

    let woundAntInTargetCell oldtarget =
        match oldtarget.Ant with
        | None -> oldtarget
        | Some (ant) ->
            let newWounds = ant.Wounds + 1 
            if newWounds >= maxAntWounds then { oldtarget with Ant = None } // Ant Dies
            else { oldtarget with Ant = Some <| ant.UpdateWounds(newWounds) }

    let getWorldChangeTransactions actions =
        seq { for source, action in actions do
                let ant = Option.get source.Ant
                match action with
                | Nothing -> ()
                | Move (target) -> 
                    if Option.isSome target.Ant then ()
                    else yield buildDependentTransaction 
                                    [ source; target.WorldCell ]
                                    [ source.Id,           (fun oldcell -> { oldcell with Ant = None });
                                    target.WorldCell.Id, (fun oldtarget -> { oldtarget with Ant = source.Ant }) ]
                | TakeFood (target) -> 
                    if target.WorldCell.Food <= 0 then ()
                    else 
                        let foodToGet = min (target.WorldCell.Food) (maxFoodAntCanCarry - ant.FoodCarried)
                        yield buildDependentTransaction
                                    [ source; target.WorldCell ]
                                    [ target.WorldCell.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food - foodToGet });
                                    source.Id,           (fun oldcell -> { oldcell with Ant = Some <| ant.UpdateFood(ant.FoodCarried + foodToGet) } ) ]
                | DropFood (target) -> 
                    if target.WorldCell.Food >= maxTotalFoodPerSquare then ()
                    else 
                        let foodToDrop = min (maxTotalFoodPerSquare - target.WorldCell.Food) (ant.FoodCarried)
                        yield buildDependentTransaction
                                    [ source; target.WorldCell ]
                                    [ target.WorldCell.Id, (fun oldtarget -> { oldtarget with Food = oldtarget.Food + foodToDrop });
                                    source.Id,           (fun oldcell -> { source with Ant = Some <| ant.UpdateFood(ant.FoodCarried - foodToDrop) }) ] 
                | DropPheromone (target, quantity) -> yield buildDependentTransaction [] [ target.WorldCell.Id, dropPheromonesInTargetCell ant.Color quantity ]
                | Attack (target) -> yield buildDependentTransaction [ source; target.WorldCell ] [ target.WorldCell.Id, woundAntInTargetCell ]
        }


    let spawnAnts (world: TheWorld) =
        world 
        |> Map.map (fun uid cell -> 
            if cell.Ant.IsNone && cell.Food >= spawnFood then
                match uid.X, uid.Y with
                | InBlackNest -> { cell with Ant = Some <| Ant(AntColor.Black); Food = cell.Food - spawnFood }
                | InRedNest -> { cell with Ant = Some <| Ant(AntColor.Red); Food = cell.Food - spawnFood }
                | Neither -> cell
            else cell)

    let degradePheromones (world: TheWorld) = 
        world 
        |> Map.map (fun uid cell -> { cell with Pheromones = cell.Pheromones |> Map.map (fun key quantity -> max (quantity - 1) 0) } )

    let applyWorldTransactions (oldWorld: TheWorld) changes = 
        Seq.fold (fun (world: TheWorld) (pred, action) ->
                    if pred world 
                    then action world  
                    else world) 
                    oldWorld changes

    let uid2xy (uid: UID) = uid.X, uid.Y

    let worldCycle bPlayer rPlayer world : TheWorld =
        world            
        |> getAntViews
        |> getAntActions bPlayer rPlayer
        |> Seq.randomPermute
        |> getWorldChangeTransactions
        |> applyWorldTransactions world
        |> degradePheromones
        |> spawnAnts

module Canvas =

    // Get the canvas context for drawing
    let canvas = document.getElementsByTagName("canvas").[0] :?> HTMLCanvasElement
    let context = canvas.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    let ($) s n = s + n.ToString()
    let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

    /// Fill rectangle with given color
    let filled (color: string) rect =
        let ctx = context
        ctx.fillStyle <- !^ color
        ctx.fillRect rect

    let drawBlob (color: string) (x, y) =
        context.beginPath()
        context.arc(x, y, 3., 0., 2. * System.Math.PI, false )
        context.fillStyle <- !^ color
        context.fill()

    /// Move element to a specified X Y position
    let position (x,y) (img : HTMLImageElement) =
        img?style?left <- x.ToString() + "px"
        img?style?top <- (canvas.offsetTop + y).ToString() + "px"

    let getWindowDimensions () =
        canvas.width, canvas.height

    /// Get the first <img /> element and set `src` (do
    /// nothing if it is the right one to keep animation)
    let image (src:string) =
        let image = document.getElementsByTagName("img").[0] :?> HTMLImageElement
        if image.src.IndexOf(src) = -1 then image.src <- src
        image

    let updateInput name (score:int) =
        let image = document.getElementsByName(name).[0] :?> HTMLInputElement
        image.value <- (string score)
        image


module Simulation =
    open Types
    open UserTypes
    open World
    open Canvas

    let drawAnt x y antColor = 
        let color = 
            match antColor with
            | AntColor.Black -> rgb 0 0 0
            | AntColor.Red -> rgb 174 0 0
        drawBlob color (x, y)

    let drawFood x y = 
        let color = rgb 0 255 0
        filled color (x, y, x + 5., y + 5.)

    let makeGradiant quantity max = (float quantity / float max)
    let drawPheromone x y antColor amount =
        let color = 
            match antColor with                    
            | AntColor.Black -> rgb 111 111 111
            | AntColor.Red -> rgb 111 0 111
        // let opacity = makeGradiant amount maxAntDropPheromoneQunatity
        filled color (x, y, x + 5., y + 5.)

    let drawUpdates (width, height) (world: TheWorld) =
        let updateCell uid cell =
            let wm, hm = width / float (xSize + 1), height / float (ySize + 1)
            let offset x y = x * wm, y * hm  
            let x, y = uid2xy uid
            let ox, oy = offset (float x) (float y)
            // cell.Pheromones |> Map.iter (fun color amount -> if amount > 0 then drawPheromone ox oy color amount)
            // if cell.Food > 0 then drawFood ox oy                                     
            if cell.Ant.IsSome then drawAnt ox oy cell.Ant.Value.Color
        world
        |> Map.iter updateCell


module HardishAI =

    open UserTypes
    open Helpers

    let rnd = System.Random(int System.DateTime.Now.Ticks)

    type TestAntBehavior() =
        interface IAntBehavior with
            member x.Name = "Rick's Hardish" 
            member x.Behave me here locations = 

                let locationsWithoutAnts = locations.EmptyCells

                let (|CarryingFood|CarryingMaxFood|CarryingNoFood|) (ant: AntView) =
                    if ant.CarryingMaxFood  then CarryingFood
                    elif ant.CarryingFood then CarryingMaxFood
                    else CarryingNoFood

                let (|NearHome|_|) (locations: AntCellView list) =
                    let homeNodes = locations |> List.filter (fun node -> node.IsMyNest)
                    if List.isEmpty homeNodes then None
                    else Some homeNodes
                
                let (|AwayFromHome|NearHome|) (locations: AntCellView list) =
                    let homeLocations, awayLocations = locations |> List.partition (fun node -> node.IsMyNest)
                    if List.isEmpty homeLocations then AwayFromHome awayLocations
                    else NearHome homeLocations 

                let (|CanDrop|CantDrop|) (locations: AntCellView list) =
                    let dropFoodLocations = locations |> List.filter (fun node -> not (node.IsFullOfFood))
                    if List.isEmpty dropFoodLocations then CantDrop
                    else CanDrop dropFoodLocations

                let (|HasUnownedFood|_|) (locations: AntCellView list) = 
                    let foodLocations = locations |> List.filter (fun node -> node.HasFood && not (node.IsMyNest))
                    if List.isEmpty foodLocations then None
                    else Some foodLocations

                let (|HasPheromonesAndNoAnt|_|) (locations: AntCellView list) =
                    let pheromoneLocations = locations |> List.filter (fun node -> node.Ant = None) |> List.filter (fun node -> node.HasFriendlyPheromone)
                    if List.isEmpty pheromoneLocations then None
                    else Some pheromoneLocations

                let (|HasNoAnt|_|) (locations: AntCellView list) =
                    let emptyLocations = locations |> List.filter (fun node -> node.Ant = None)
                    if List.length emptyLocations > 0 then
                        Some (emptyLocations)
                    else None
                
                let (|ShortestDistanceWithNoAnt|_|)  (locations: AntCellView list) =
                    let noAnts = locations |> List.filter (fun node -> node.Ant = None)
                    if List.length noAnts > 0 then Some (noAnts |> List.minBy (fun node -> node.DistanceToNest))
                    else None

                let maxFood = List.maxBy (fun (node: AntCellView) -> node.FoodContained)
                let minPhero = List.minBy (fun (node: AntCellView) -> node.FriendlyPheromoneQuantity)
                let noAnts = List.filter (fun (node: AntCellView) -> node.ContainsAnt)


                // [snippet:Simple Pheromone-Using Ant Colony AI]
                match me with
                | CarryingFood
                | CarryingMaxFood -> 
                    match locations.Cells with                    
                    | NearHome homeCells -> 
                        match homeCells with
                        | CanDrop dropCells -> DropFood dropCells.Head
                        | HasNoAnt noAntCells -> Move (List.random noAntCells)
                        | _ -> Nothing
                    | AwayFromHome allCells -> 
                        match here.FriendlyPheromoneQuantity with
                        | n when n < 20 -> DropPheromone (here, 100 - n)
                        | _ -> 
                            match allCells with
                            | HasNoAnt noAnts when rnd.Next(0, 3) = 0 -> Move (List.random noAnts)
                            | ShortestDistanceWithNoAnt node -> Move node
                            | _ -> Nothing
                | CarryingNoFood -> 
                    match locations.Cells with
                    | HasNoAnt noAnts when rnd.Next(0, 3) = 0 -> Move (List.random noAnts)                        
                    | HasUnownedFood foodCells -> TakeFood (maxFood foodCells)
                    | HasPheromonesAndNoAnt pheroCells -> Move (minPhero pheroCells)
                    | HasNoAnt noAntCells -> Move (List.random noAntCells)
                    | _ -> Nothing



module AntsEverywhereExmampleAI =
    open UserTypes

    let randomGen = new System.Random()

    let getRandomVal min max = 
        lock randomGen (fun () -> randomGen.Next(min, max))

    type TestAntBehavior() =
        interface IAntBehavior with
            member x.Name = "Frank_Levine"
            member x.Behave me here locations =
            
                // This Ant's basic strategy is this:
                // If you have food and are near the nest
                //      drop the food
                // If you can't carry anymore food (bur are not near the nest)
                //      head back to the nest with the following exception
                //          if the current cell (here) has <40 phereomones, replenish the supply back to 100
                // If you're not dropping off food or heading home, you're foraging
                //      The logic for foraging is:
                //      If you see food, take it (this applies even when you have food but aren't full)
                //      If you see pheromones, move to the pheromone that is farthest from the nest
                //          if all pheromones are closer to the nest than you, then make a random move
                //      Otherwise you'e in the middle of nowhere, wanter randomly
                //
                // Special note on 'Traffic Control':  Inbound ants always yield to outbound ants
                //                                     This seems reasonable since the inbound ants
                //                                     Know where they're going and the outbound ones
                //                                     Are dependent on the pheromone trail
            
            
            
                //                                    
                // helper functions
                let isNest (cell: AntCellView) = cell.IsMyNest
            
                // how do I negate a function?!?  this seems a bit heavy-handed
                let isNotNest (cell: AntCellView) =
                    if isNest cell then
                        false
                    else
                        true

                // nest cells that can receive food
                let nestCells = locations.FoodDropCells

                // all empty neighbors, sorted so we can get at the closest and farthest ones from the nest
                // first = closest to nest
                // last = farthest from nest
                let emptyNeighbors = locations.EmptyCells |> List.sortBy (fun c -> c.DistanceToNest)                                     

                // all empty neighbors with my pheromones
                let emptyNeighborsWithP = locations.FriendlyPheromoneCells |> List.sortBy( fun c -> c.DistanceToNest) |> List.toArray

                // all neighbors with food, ordered by the amount of food decending
                let neighborsWithFood = locations.FoodCollectionCells
                                        |> List.sortBy (fun c -> -c.FoodContained)

                // functions to make the code below more readable
                // NullMove does nothing (like when you're boxed in)
                // RandomMove is... Random
                let NullMove = fun() -> Move here

                let RandomMove = fun () ->
                    let i = getRandomVal 0 emptyNeighbors.Length
                    Move (List.item i emptyNeighbors)


                // maximum amount of pheromone to leave on a cell
                let MAX_PHERO = 100;
            
                // when returning to the nest, add more pheromones when the cell
                // has less than this number
                let REFRESH_THRESHOLD = 50;

                // active pattern to determine the ant's high-level state           
                let (|ShouldDropFood|Forage|ReturnToNest|) (ant: AntView) =
                    let haveAvailableNestCells = (nestCells.IsEmpty = false)
                    match ant with
                        | a when a.CarryingFood && haveAvailableNestCells -> ShouldDropFood
                        | a when a.CarryingMaxFood -> ReturnToNest
                        | _ -> Forage

                // active pattern to decide if we need to refresh pheromones
                let (|NeedsRefresh|NoRefresh|) (cell: AntCellView) =
                    match cell.FriendlyPheromoneQuantity with
                        | x when x < REFRESH_THRESHOLD ->
                            let amt = MAX_PHERO - x     // amt is the number of pheromones required to bring this cell back to 100
                            NeedsRefresh amt
                        | _ -> NoRefresh    // there are enough for now

                // gets the relative distance to the nest
                // relativeDist > 0 --> cell is farther from the nest than 'here'
                // relativeDist < 0 --> cell is closer to the nest than 'here'                   
                let relativeDist (cell: AntCellView) =
                    let dHere = here.DistanceToNest
                    let dCell = cell.DistanceToNest
                    dCell - dHere

                // function to get the last thing from an array
                let last (arr: 'a[]) =
                    arr.[arr.Length-1]

                // the ant parameter isn't used, but I don't know how to make a
                // parameterless active pattern
                let (|AdjacentToFood|AdjacentToPheromone|NoMansLand|) (ant: AntView) =
                    if neighborsWithFood.Length > 0 then
                        AdjacentToFood
                    elif emptyNeighborsWithP.Length > 0 && relativeDist (last emptyNeighborsWithP) > 0. then   
                        // remember emptyNeighborsWithP is sorted
                        AdjacentToPheromone (last emptyNeighborsWithP)
                    else
                        NoMansLand

                // The Actual logic...

                if emptyNeighbors.IsEmpty then
                    NullMove()
                else
                    match me with
                    | ShouldDropFood -> DropFood nestCells.Head               
                    | ReturnToNest ->
                        match here with
                        | NeedsRefresh amt -> DropPheromone (here, amt)                   
                        | NoRefresh -> Move emptyNeighbors.Head
                    | Forage ->
                        match me with
                        | AdjacentToFood -> TakeFood neighborsWithFood.Head                   
                        | AdjacentToPheromone pheroCell -> Move pheroCell
                        | NoMansLand -> RandomMove()

open Canvas
open Types
open UserTypes
open World
open Simulation

let origin =
    // Sample is running in an iframe, so get the location of parent
    let topLocation = window.top.location
    topLocation.origin + topLocation.pathname

let maxCycles = 1000
let world = ref (buildWorldInitialWorld())
let foodToWin = int <| double (Map.fold (fun s k v -> s + v.Food) 0 !world) * percentFoodToWin
let cycles = ref 0

let blackAI = new HardishAI.TestAntBehavior()
let redAI = new AntsEverywhereExmampleAI.TestAntBehavior()

let render (w,h) =
    // console.log(sprintf "%A" (w,h))
    cycles := !cycles + 1

    let mutable bScore = 0
    let mutable rScore = 0
    for (k,v) in !world |> Map.toSeq do
        match v.Ant with
        | None -> ()
        | Some (ant) -> 
            if ant.Color = AntColor.Black then bScore <- bScore + 1
            elif ant.Color = AntColor.Red then rScore <- rScore + 1

    updateInput "cycles" !cycles |> ignore
    updateInput "bscore" bScore |> ignore
    updateInput "rscore" rScore |> ignore

    if bScore = 0 || rScore = 0 || !cycles > maxCycles then
        if bScore > rScore then raise (new System.Exception("Black"))
        elif rScore > bScore then raise (new System.Exception("red"))

    (0., 0., w, h) |> filled (rgb 174 238 238)
    drawUpdates (w,h) !world
    world := worldCycle blackAI redAI !world

let w, h = getWindowDimensions()

let rec update () =
    render (w,h) 
    window.setTimeout(update, 1000 / 6) |> ignore

update ()
