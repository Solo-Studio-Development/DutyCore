package net.solostudio.dutycore.utils;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.annotations.Badges;
import net.solostudio.dutycore.annotations.Staffs;
import net.solostudio.dutycore.commands.CommandDuty;
import net.solostudio.dutycore.enums.keys.ConfigKeys;
import net.solostudio.dutycore.exception.CommandExceptionHandler;
import net.solostudio.dutycore.listeners.DatabaseListener;
import net.solostudio.dutycore.listeners.DutyListener;
import net.solostudio.dutycore.listeners.MenuListener;
import org.bukkit.Bukkit;
import revxrsal.commands.bukkit.BukkitLamp;
import revxrsal.commands.orphan.Orphans;

public class RegisterUtils {
    public static void registerListeners() {
        LoggerUtils.info("### Registering listeners... ###");

        Bukkit.getPluginManager().registerEvents(new DatabaseListener(), DutyCore.getInstance());
        Bukkit.getPluginManager().registerEvents(new DutyListener(), DutyCore.getInstance());
        Bukkit.getPluginManager().registerEvents(new MenuListener(), DutyCore.getInstance());

        LoggerUtils.info("### Successfully registered 3 listener. ###");
    }

    public static void registerCommands() {
        LoggerUtils.info("### Registering commands... ###");

        var lamp = BukkitLamp.builder(DutyCore.getInstance())
                .exceptionHandler(new CommandExceptionHandler())
                .suggestionProviders(providers -> providers.addProviderForAnnotation(Staffs.class, staffs -> context -> DutyCore.getDatabase().getEveryStaffInDatabase()
                        .stream()
                        .toList()))
                .suggestionProviders(providers -> providers.addProviderForAnnotation(Badges.class, badges -> context -> StartingUtils.getBadges().keySet()))
                .build();

        lamp.register(Orphans.path(ConfigKeys.ALIASES.getList().toArray(String[]::new)).handler(new CommandDuty()));

        LoggerUtils.info("### Successfully registered exception handlers... ###");
    }
}
