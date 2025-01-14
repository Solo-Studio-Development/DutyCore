package net.solostudio.dutycore.commands;

import lombok.Getter;
import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.jetbrains.annotations.NotNull;
import revxrsal.commands.annotation.Flag;
import revxrsal.commands.annotation.Subcommand;
import revxrsal.commands.bukkit.annotation.CommandPermission;
import revxrsal.commands.orphan.OrphanCommand;

import java.util.ArrayList;
import java.util.List;

public class CommandDuty implements OrphanCommand {
    @Getter public static List<Player> freezeList = new ArrayList<>();

    @Subcommand("join")
    @CommandPermission("dutycore.staff")
    public void join(@NotNull Player player) {
        DutyCore.getDatabase().joinDuty(player.getName());
        player.sendMessage("Beléptél Dutyba!");
    }

    @Subcommand("leave")
    @CommandPermission("dutycore.staff")
    public void leave(@NotNull Player player) {
        DutyCore.getDatabase().leaveDuty(player.getName());
        player.sendMessage("Kiléptél a Dutyból!");
        player.sendMessage(DutyCore.getDatabase().getFormattedDutyTime(player.getName()));
    }

    @Subcommand("chat")
    @CommandPermission("dutycore.staff")
    public void chat(@NotNull Player sender, @NotNull @Flag(shorthand = 'a') String message) {
        if (!DutyCore.getDatabase().isInDuty(sender.getName())) {
            sender.sendMessage("Dutyba kell lenned!");
            return;
        }

        Bukkit.getOnlinePlayers().stream()
                .filter(player -> player.hasPermission("dutycore.staff"))
                .filter(player -> DutyCore.getDatabase().isInDuty(player.getName()))
                .forEach(player -> player.sendMessage(sender.getName() + " > " + message));
    }


    @Subcommand("freeze")
    @CommandPermission("dutycore.staff")
    public void freeze(@NotNull Player player, @NotNull Player target) {

        if (getFreezeList().contains(target)) {
            getFreezeList().remove(target);
            player.sendMessage("Levetted");
            target.sendMessage("Freezelve levéve");
        } else {
            getFreezeList().add(target);
            player.sendMessage("Sikeres freeze");
            target.sendMessage("Freezelve lettél!");
        }
    }
}
