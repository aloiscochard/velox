extern crate colored;

use colored::*;
use pathdiff::diff_paths;
use std::fs::canonicalize;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, SystemTime};

// TODO Make those configurable
const ATTACK: u128 = 100; // in ms
const FRAME_DURATION: u64 = 7; // in ms

// TODO Add command line arguments parsing for the tool itself
// TODO Command line argument use regex pattern to match file (orthogonal to .gitignore) -- https://docs.rs/gitignore/1.0.7/gitignore/struct.Pattern.html
// TODO Read and use global gitignore -- https://docs.rs/ignore/0.4.17/src/ignore/gitignore.rs.html#128-130
// TODO Use rosetta.sources (orthogonal to .gitignores)

fn main() {
    let mut args = std::env::args();
    let mut cmd = String::new();

    if let Some(_) = args.next() {
        if let Some(arg) = args.next() {
            cmd.push_str(&arg);

            for arg in args {
                cmd.push(' ');
                cmd.push_str(&arg);
            }
        }
    }

    let timeout = Duration::from_millis(FRAME_DURATION);
    let running = Arc::new(AtomicBool::new(true));
    let watching = running.clone();

    let (watcher_tx, watcher_rx): (Sender<PathBuf>, Receiver<PathBuf>) = channel();

    let watcher_handle = thread::spawn(move || {
        use notify::{RecommendedWatcher, RecursiveMode, Watcher};

        let root_relative = Path::new("./");
        let root = canonicalize(&root_relative).expect("can not resolve working directory");
        let gitignore_path = root.join(".gitignore");

        let mut gitignores: Vec<gitignore::File> = Vec::new();

        if gitignore_path.exists() {
            match gitignore::File::new(&gitignore_path) {
                Ok(gi) => gitignores.push(gi),
                Err(err) => {
                    println!("warning: unable to read gitignore.\n\t{:?}", err);
                }
            }
        };

        let (tx, rx) = channel();
        let mut watcher: RecommendedWatcher = Watcher::new_raw(tx).unwrap();
        watcher.watch(&root, RecursiveMode::Recursive).unwrap();

        while watching.load(Ordering::Relaxed) {
            match rx.recv_timeout(timeout) {
                Ok(event) => {
                    let path = event.path.unwrap();
                    let mut included: bool = true;

                    for gi in &gitignores {
                        included = !gi
                            .is_excluded(&path)
                            .expect("failure when loading gitignore patterns");

                        if !included {
                            break;
                        }
                    }

                    if included {
                        watcher_tx
                            .send(path)
                            .expect("failure when sending path to channel");
                    }
                }
                Err(_) => {} // NOOP
            }
        }
    });

    {
        let r = running.clone();
        ctrlc::set_handler(move || {
            r.store(false, Ordering::SeqCst);
        })
        .expect("Error setting Ctrl-C handler");
    }

    let root = std::env::current_dir().expect("can't fetch current directory from the environment");
    let title = "vlx".cyan();

    // TODO Make looping here configurable (so it can could after first trigger)
    while running.load(Ordering::SeqCst) {
        let triggered_at = SystemTime::now();
        let mut child = Command::new("script")
            .args(&["-qec", &cmd, "/dev/null"])
            .spawn()
            .expect("command failed to start");

        let mut triggered = None;
        while running.load(Ordering::SeqCst) {
            let was_triggered = triggered.is_some();
            for path in watcher_rx.recv_timeout(timeout) {
                let update = match triggered_at.elapsed() {
                    Ok(d) if d.as_millis() > ATTACK => true,
                    Err(_) => true, // time drifting backwards
                    _ => false,
                };

                if update {
                    triggered = Some(diff_paths(&path, &root).unwrap());
                }
            }
            if was_triggered {
                break;
            }
        }

        match child.try_wait() {
            _ => {} // NOOP
        }

        match child.kill() {
            _ => {} // NOOP
        };

        for path in triggered {
            println!(
                "{}: triggered by {}",
                title,
                path.display().to_string().cyan()
            );

            while watcher_rx.try_recv().is_ok() {}
        }
    }

    watcher_handle
        .join()
        .expect("failure when waiting for watcher to complete");
}
