import com.lib.DataManagment.{DataLoader, DataUpdater}

object Main extends App {
 val DataManager = DataLoader.loadData("src/resources/users.csv", "src/resources/units.csv")

 DataUpdater.update(DataManager.getUsers, "src/resources/users.csv")
 DataUpdater.update(DataManager.getUnits, "src/resources/units.csv")
}